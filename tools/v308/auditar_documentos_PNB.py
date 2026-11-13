"""Presenca e escala fisica das paginas no DOCX/PDF produtivos; sem Word COM."""
import argparse
import base64
import csv
import hashlib
import io
import json
from pathlib import Path
import re
import urllib.parse
from zipfile import ZipFile
from xml.etree import ElementTree as ET

from pypdf import PdfReader
from pypdf.generic import ContentStream
from PIL import Image


def sha(data):
    return hashlib.sha256(data).hexdigest()


def rows(path):
    with path.open(encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def audit(report, output, stem="analitico_detalhado"):
    report = Path(report)
    index = rows(report / "indice_selecao_graficos.csv")
    expected = []
    for theme in index:
        if not theme["id"].startswith("inferencias_") or theme.get("incorporada_relatorio_detalhado") != "TRUE":
            continue
        canonical = report / theme["arquivo_relatorio"]
        key = hashlib.sha256(canonical.name.encode("utf-8")).hexdigest()[:16]
        manifest = canonical.parent / f"ev_{key}_paginas.csv"
        pages = rows(manifest)
        for page in pages:
            path = canonical.parent / page["arquivo"]
            data = path.read_bytes()
            assert sha(data) == page["sha256"]
            image = Image.open(io.BytesIO(data)).convert("RGB")
            expected.append({"theme_id": theme["id"], "path": path.relative_to(report).as_posix(),
                "sha256": sha(data), "pixel_md5": hashlib.md5(image.tobytes()).hexdigest(),
                "width_px": image.width, "height_px": image.height,
                "source_width_in": float(page["largura_pol"]), "source_height_in": float(page["altura_pol"]),
                "source_font_pt": float(page["fonte_pt"]), "results": int(page["n_resultados"]),
                "page_in_family": int(page["pagina"]), "family_pages": int(page["n_paginas"])})
    assert expected, "No selected evidence pages"
    checks = []
    def check(name, ok, detail=None):
        checks.append({"name": name, "pass": bool(ok), "detail": detail})
    paths = [r["path"] for r in expected]
    for ext in ["Rmd", "md", "html"]:
        file = report / f"{stem}.{ext}"
        text = file.read_text(encoding="utf-8-sig")
        srcs = [urllib.parse.unquote(s) for s in re.findall(r'<img\b[^>]*\bsrc=["\']([^"\']+)', text)]
        embedded = []
        for src in srcs:
            if src.startswith("data:image/png;base64,"):
                embedded.append(sha(base64.b64decode(src.split(",", 1)[1])))
        counts = {item["path"]:srcs.count(item["path"])+embedded.count(item["sha256"]) for item in expected}
        check(f"all_pages_once_{ext}", all(v == 1 for v in counts.values()),
              {path:n for path,n in counts.items() if n != 1})
        check(f"semantic_references_resolved_{ext}", "[[figura:" not in text)
    w = "{http://schemas.openxmlformats.org/wordprocessingml/2006/main}"
    wp = "{http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing}"
    a = "{http://schemas.openxmlformats.org/drawingml/2006/main}"
    r = "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}"
    by_sha = {}
    for item in expected:
        by_sha.setdefault(item["sha256"], []).append(item)
        item["docx_occurrences"] = []
        item["pdf_occurrences"] = []
    with ZipFile(report / f"{stem}.docx") as z:
        rels = {e.get("Id"): e.get("Target") for e in ET.fromstring(z.read("word/_rels/document.xml.rels"))}
        document = ET.fromstring(z.read("word/document.xml"))
        for drawing in document.iter(w + "drawing"):
            extent = drawing.find(".//" + wp + "extent")
            blip = drawing.find(".//" + a + "blip")
            if extent is None or blip is None:
                continue
            target = rels.get(blip.get(r + "embed"))
            if not target:
                continue
            part = target.lstrip("/") if target.startswith("/") else "word/" + target
            image_hash = sha(z.read(part))
            for item in by_sha.get(image_hash, []):
                width = int(extent.get("cx")) / 914400
                height = int(extent.get("cy")) / 914400
                sx = width / item["source_width_in"]
                sy = height / item["source_height_in"]
                crop = drawing.find(".//" + a + "srcRect")
                cropped = crop is not None and any(int(v) != 0 for v in crop.attrib.values())
                item["docx_occurrences"].append({"width_in": width, "height_in": height,
                    "scale_x": sx, "scale_y": sy, "minimum_font_pt_after_scale": item["source_font_pt"] * min(sx, sy),
                    "cropped": cropped, "source_png_sha_exact": True})
    pdf = PdfReader(report / f"{stem}.pdf")
    by_pixels = {}
    for item in expected:
        by_pixels.setdefault(item["pixel_md5"], []).append(item)
    def mult(m, n):
        return [m[0]*n[0]+m[1]*n[2], m[0]*n[1]+m[1]*n[3],
                m[2]*n[0]+m[3]*n[2], m[2]*n[1]+m[3]*n[3],
                m[4]*n[0]+m[5]*n[2]+n[4], m[4]*n[1]+m[5]*n[3]+n[5]]
    for page_index, page in enumerate(pdf.pages):
        image_hashes = {}
        for image in page.images:
            ref = image.indirect_reference
            if ref is not None:
                rgb = image.image.convert("RGB")
                image_hashes[(ref.idnum, ref.generation)] = hashlib.md5(rgb.tobytes()).hexdigest()
        def inspect(stream, resources, matrix, depth=0):
            if depth > 32:
                raise AssertionError("Recursive PDF form")
            ctm = list(matrix); stack = []
            for operands, operator in ContentStream(stream, pdf).operations:
                if operator == b"q":
                    stack.append(list(ctm))
                elif operator == b"Q":
                    ctm = stack.pop()
                elif operator == b"cm":
                    ctm = mult([float(v) for v in operands], ctm)
                elif operator == b"Do":
                    refs = resources.get("/XObject", {})
                    reference = refs[operands[0]]
                    obj = reference.get_object()
                    if obj.get("/Subtype") == "/Form":
                        inspect(obj, obj.get("/Resources", resources), mult([float(v) for v in obj.get("/Matrix", [1,0,0,1,0,0])], ctm), depth+1)
                    elif obj.get("/Subtype") == "/Image":
                        ref = getattr(obj, "indirect_reference", None)
                        if ref is None:
                            continue
                        digest = image_hashes.get((ref.idnum, ref.generation))
                        matches = by_pixels.get(digest, [])
                        xs = [ctm[0]*x+ctm[2]*y+ctm[4] for x,y in [(0,0),(1,0),(0,1),(1,1)]]
                        ys = [ctm[1]*x+ctm[3]*y+ctm[5] for x,y in [(0,0),(1,0),(0,1),(1,1)]]
                        box = [min(xs), min(ys), max(xs), max(ys)]
                        for item in matches:
                            sx = (box[2]-box[0])/72/item["source_width_in"]
                            sy = (box[3]-box[1])/72/item["source_height_in"]
                            inside = box[0]>=-.25 and box[1]>=-.25 and box[2]<=float(page.mediabox.width)+.25 and box[3]<=float(page.mediabox.height)+.25
                            item["pdf_occurrences"].append({"pdf_page":page_index+1, "bbox_pt":box,
                                "scale_x":sx, "scale_y":sy, "minimum_font_pt_after_scale":item["source_font_pt"]*min(sx,sy),
                                "inside_page":inside, "source_pixel_hash_exact":True})
        inspect(page.get_contents(), page["/Resources"], [1,0,0,1,0,0])
    for item in expected:
        check("docx_page_present_once:" + item["path"], len(item["docx_occurrences"]) == 1)
        check("pdf_page_present_once:" + item["path"], len(item["pdf_occurrences"]) == 1)
        for kind in ["docx", "pdf"]:
            for occurrence in item[kind + "_occurrences"]:
                check(kind + "_font_at_least_7pt:" + item["path"], occurrence["minimum_font_pt_after_scale"] >= 7 - 1e-4)
                check(kind + "_aspect_preserved:" + item["path"], abs(occurrence["scale_x"] / occurrence["scale_y"] - 1) < .002)
                check(kind + "_not_cropped:" + item["path"], occurrence.get("inside_page", not occurrence.get("cropped", False)))
    result = {"pass": all(x["pass"] for x in checks), "report": str(report), "stem": stem,
        "expected_families": len({x["theme_id"] for x in expected}), "expected_pages": len(expected),
        "expected_results": sum(x["results"] for x in expected), "pdf_pages": len(pdf.pages),
        "checks": checks, "figures": expected,
        "method": "DOCX embedded PNG SHA + wp:extent; PDF decoded RGB pixels + physical image bounding boxes; no COM",
        "limitations": "Word page-flow and manual table-of-contents destinations are not proved by OOXML image extents."}
    Path(output).write_text(json.dumps(result, ensure_ascii=False, indent=2), encoding="utf-8")
    print(json.dumps({"pass": result["pass"], "families": result["expected_families"],
        "png_pages": result["expected_pages"], "results": result["expected_results"],
        "checks": len(checks), "failed": [x for x in checks if not x["pass"]][:8]}, ensure_ascii=True))
    return result


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("report")
    ap.add_argument("output")
    ap.add_argument("--stem", default="analitico_detalhado")
    ns = ap.parse_args()
    result = audit(ns.report, ns.output, ns.stem)
    raise SystemExit(0 if result["pass"] else 1)
