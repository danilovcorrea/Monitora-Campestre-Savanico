#!/usr/bin/env python3
import csv
import hashlib
import math
import sys
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET


if len(sys.argv) != 3:
    raise SystemExit("Uso: qa_validate_kml_v280.py REGISTROS_CORRIG_STAT OUTPUT_DIR")

csv_path = Path(sys.argv[1])
spatial_dir = Path(sys.argv[2]) / "04_validacao_espacial"
ns = {"k": "http://www.opengis.net/kml/2.2"}


def require(condition, message):
    if not condition:
        raise AssertionError(message)


def parse_kml(name):
    path = spatial_dir / name
    require(path.is_file() and path.stat().st_size > 0, f"arquivo ausente/vazio: {path}")
    return ET.parse(path).getroot()


def schema_fields(root):
    return {
        node.attrib.get("name", "")
        for node in root.findall(".//k:Schema/k:SimpleField", ns)
    }


def haversine_m(lon1, lat1, lon2, lat2):
    radius = 6371008.8
    p1, p2 = math.radians(lat1), math.radians(lat2)
    dlat = p2 - p1
    dlon = math.radians(lon2 - lon1)
    a = math.sin(dlat / 2) ** 2 + math.cos(p1) * math.cos(p2) * math.sin(dlon / 2) ** 2
    return 2 * radius * math.asin(min(1.0, math.sqrt(a)))


with csv_path.open("r", encoding="utf-8-sig", newline="") as handle:
    reader = csv.DictReader(handle)
    csv_fields = reader.fieldnames or []
    rows = list(reader)

valid_rows = []
for row in rows:
    try:
        lon1, lat1 = float(row["long_ini"]), float(row["lat_ini"])
        lon2, lat2 = float(row["long_fin"]), float(row["lat_fin"])
    except (KeyError, TypeError, ValueError):
        continue
    if all(math.isfinite(x) for x in (lon1, lat1, lon2, lat2)) and -180 <= lon1 <= 180 and -180 <= lon2 <= 180 and -90 <= lat1 <= 90 and -90 <= lat2 <= 90:
        valid_rows.append((lon1, lat1, lon2, lat2))

n = len(valid_rows)
require(n > 0, "fixture sem coordenadas válidas")

field_root = parse_kml("UAs_verg_ini_verg_fin.kml")
stat_root = parse_kml("UAs_registros_corrig_stat.kml")
area_root = parse_kml("UAs_areas_operacionais_protecao_100m.kml")

expected_field = {
    "UC", "UA", "COLETA", "ANO", "PROTOCOLO", "CICLO", "CAMPANHA", "form_veg",
    "lat_ini", "long_ini", "alt_ini", "acc_ini", "lat_fin", "long_fin", "alt_fin", "acc_fin",
    "point_type",
}.intersection(set(csv_fields) | {"point_type"})
field_schema = schema_fields(field_root)
stat_schema = schema_fields(stat_root)
area_schema = schema_fields(area_root)

require(field_schema == expected_field, f"schema compartilhável divergente: {sorted(field_schema ^ expected_field)}")
require("form_veg" in field_schema, "form_veg ausente do KML compartilhável")
require(".id" not in field_schema, ".id indevidamente exposto no KML compartilhável")
require(stat_schema == set(csv_fields) | {"point_type"}, f"KML estatístico perdeu/ganhou campos: {sorted(stat_schema ^ (set(csv_fields) | {'point_type'}))}")
require(".id" in stat_schema, ".id ausente do produto estatístico restrito")
require("form_veg" in area_schema, "form_veg ausente da área operacional")
require(".id" not in area_schema, ".id indevidamente exposto na área operacional")
require("ano_referencia_espacial" in area_schema, "ano de referência espacial ausente da área operacional")
require("referencia_espacial" not in area_schema, "ordinal ambíguo de referência espacial ainda presente")
require("total_referencias_espaciais_ua" in area_schema, "total de referências espaciais da UA ausente")

for label, root in (("campo", field_root), ("estatístico", stat_root)):
    require(len(root.findall(".//k:LineString", ns)) == n, f"contagem de linhas incorreta no KML {label}")
    require(len(root.findall(".//k:Point", ns)) == 2 * n, f"contagem de pontos incorreta no KML {label}")
    require(len(root.findall(".//k:Placemark", ns)) == 3 * n, f"contagem de placemarks incorreta no KML {label}")
    folders = [node.text for node in root.findall(".//k:Folder/k:name", ns)]
    require(folders == ["Transectos", "Vértices inicial e final"], f"pastas incorretas no KML {label}: {folders}")

point_style = field_root.find(".//k:Style[@id='ponto_ua']", ns)
line_style = field_root.find(".//k:Style[@id='linha_ua']", ns)
require(point_style is not None and line_style is not None, "estilos de pontos/linhas ausentes")
require(point_style.findtext("k:IconStyle/k:Icon/k:href", namespaces=ns) == "files/icon1.png", "referência do ícone divergente")
require(point_style.findtext("k:IconStyle/k:scale", namespaces=ns) == "0.5039370078740157", "escala do ícone divergente")
require(line_style.findtext("k:LineStyle/k:color", namespaces=ns) == "ff1c1ae3", "cor da transecção divergente")
require(line_style.findtext("k:LineStyle/k:width", namespaces=ns) == "0.7358267716535433", "largura da transecção divergente")

area_style = area_root.find(".//k:Style[@id='area_protecao_ua']", ns)
require(area_style is not None, "estilo da área operacional ausente")
require(area_style.findtext("k:LineStyle/k:color", namespaces=ns) == "ff00ffff", "contorno da área não é amarelo")
require(area_style.findtext("k:PolyStyle/k:fill", namespaces=ns) == "0", "área operacional possui preenchimento")
area_placemarks = area_root.findall(".//k:Placemark", ns)
require(len(area_placemarks) > 0, "nenhuma área operacional materializada")
require(len(area_root.findall(".//k:Polygon", ns)) == len(area_placemarks), "contagem de polígonos divergente")
require(len(area_root.findall(".//k:Point", ns)) == len(area_placemarks), "pontos de rótulo das áreas ausentes")

max_radius_error = 0.0
for placemark in area_placemarks:
    label = placemark.findtext("k:name", default="", namespaces=ns)
    require(label.startswith("UA-") and "Área operacional" not in label and "referência" not in label, f"rótulo operacional excessivo: {label}")
    center_text = placemark.findtext(".//k:Point/k:coordinates", namespaces=ns)
    ring_text = placemark.findtext(".//k:LinearRing/k:coordinates", namespaces=ns)
    require(center_text and ring_text, "área sem centro ou anel")
    center = [float(x) for x in center_text.strip().split(",")[:2]]
    ring = [[float(x) for x in token.split(",")[:2]] for token in ring_text.strip().split()]
    require(len(ring) == 73, f"anel com {len(ring)} vértices em vez de 73")
    require(haversine_m(ring[0][0], ring[0][1], ring[-1][0], ring[-1][1]) < 0.001, "anel não fechado")
    for lon, lat in ring:
        max_radius_error = max(max_radius_error, abs(haversine_m(center[0], center[1], lon, lat) - 100.0))
require(max_radius_error < 0.01, f"erro máximo do raio excede tolerância: {max_radius_error:.6f} m")

icon = spatial_dir / "files" / "icon1.png"
require(icon.is_file(), "ícone lateral ausente")
require(hashlib.sha256(icon.read_bytes()).hexdigest() == "21effb09e7395e06ca45b43f05c031e60e2ed0a490476a990d4e3dc08825200b", "ícone diverge do template")

for stem in ("UAs_verg_ini_verg_fin", "UAs_registros_corrig_stat", "UAs_areas_operacionais_protecao_100m"):
    kmz = spatial_dir / f"{stem}.kmz"
    require(kmz.is_file() and kmz.stat().st_size > 0, f"KMZ ausente/vazio: {kmz}")
    with zipfile.ZipFile(kmz) as archive:
        require(set(archive.namelist()) == {"doc.kml", "files/icon1.png"}, f"estrutura interna divergente em {kmz.name}: {archive.namelist()}")
        ET.fromstring(archive.read("doc.kml"))
        require(hashlib.sha256(archive.read("files/icon1.png")).hexdigest() == hashlib.sha256(icon.read_bytes()).hexdigest(), f"ícone interno divergente em {kmz.name}")

print(
    "QA_KML_ESTRUTURAL_OK",
    f"n_transectos={n}",
    f"n_areas={len(area_placemarks)}",
    f"campos_campo={len(field_schema)}",
    f"campos_stat={len(stat_schema)}",
    f"erro_raio_max_m={max_radius_error:.9f}",
)
