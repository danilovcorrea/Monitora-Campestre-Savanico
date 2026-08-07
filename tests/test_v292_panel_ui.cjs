const fs = require("fs");
const path = require("path");
const playwrightModule = process.argv[4] || "playwright";
const { chromium } = require(playwrightModule);

const url = process.argv[2] || "http://127.0.0.1:43193";
const outputDir = process.argv[3] || process.cwd();
fs.mkdirSync(outputDir, { recursive: true });

const must = (condition, message) => {
  if (!condition) throw new Error(message);
};

(async () => {
  const launchOptions = { headless: true };
  if (process.env.CHROME_BIN) launchOptions.executablePath = process.env.CHROME_BIN;
  const browser = await chromium.launch(launchOptions);
  const page = await browser.newPage({ viewport: { width: 1600, height: 1000 } });
  const browserErrors = [];
  page.on("pageerror", (error) => browserErrors.push(error.stack || String(error)));
  page.on("console", (message) => {
    if (message.type() === "error") browserErrors.push(message.text());
  });

  try {
    await page.goto(url, { waitUntil: "domcontentloaded", timeout: 60000 });
    await page.getByText("Justificar pendências", { exact: true }).waitFor({ timeout: 60000 });
    await page.waitForTimeout(4000);

    const bodyInitial = await page.locator("body").innerText();
    must(bodyInitial.includes("Validação espacial"), "aba de validação espacial ausente");
    must(bodyInitial.includes("Justificar pendências"), "aba de justificativas ausente");
    must(bodyInitial.includes("7 para revisão de formação vegetacional na mesma UA") || bodyInitial.includes("formação vegetacional"), "diagnóstico de formação vegetacional não visível");
    await page.screenshot({ path: path.join(outputDir, "painel_inicial.png"), fullPage: true });

    await page.getByText("Validação espacial", { exact: true }).click();
    await page.locator("#esp_modo_fluxo").waitFor({ timeout: 30000 });
    await page.waitForTimeout(1200);
    const bodySpatial = await page.locator(".tab-pane.active").innerText();
    must(bodySpatial.includes("Correção espacial: origem → destino → operação"), "fluxo espacial unificado ausente");
    must(!bodySpatial.includes("Número esperado de linhas-alvo"), "campo obsoleto de linhas esperadas ainda visível");
    must(bodySpatial.includes("Uma ou mais COLETAS") && bodySpatial.includes("Lote entre ANOS"), "modos espaciais unificados ausentes");

    await page.locator("#esp_coord_inicio_nova").fill("-13.9000, -41.1000");
    await page.locator("#esp_coord_fim_nova").fill("-13.9010, -41.1010");
    const stateBefore = await page.evaluate(() => {
      const firstOption = (id) => {
        const el = document.getElementById(id);
        if (!el || !el.selectize) return null;
        const key = Object.keys(el.selectize.options).find((value) => String(value).trim() !== "");
        if (key) el.selectize.setValue(key);
        return key || null;
      };
      return {
        spatialYear: firstOption("esp_filtro_ano"),
        collection: firstOption("coleta"),
        generalYear: firstOption("filtro_ano"),
      };
    });
    await page.waitForTimeout(1200);
    await page.locator("#limpar_filtros").click();
    await page.waitForTimeout(1800);
    const stateAfter = await page.evaluate(() => {
      const value = (id) => {
        const el = document.getElementById(id);
        if (!el) return null;
        if (el.selectize) return el.selectize.getValue();
        return el.value;
      };
      return {
        spatialYear: value("esp_filtro_ano"),
        collection: value("coleta"),
        generalYear: value("filtro_ano"),
        start: value("esp_coord_inicio_nova"),
        end: value("esp_coord_fim_nova"),
      };
    });
    const empty = (value) => value === null || value === "" || (Array.isArray(value) && value.length === 0);
    must(empty(stateAfter.spatialYear), "limpeza geral não apagou filtro espacial de ANO");
    must(empty(stateAfter.collection), "limpeza geral não apagou COLETA a corrigir");
    must(empty(stateAfter.generalYear), "limpeza geral não apagou filtro geral de ANO");
    must(empty(stateAfter.start) && empty(stateAfter.end), "limpeza geral não apagou coordenadas pré-preenchidas");
    await page.screenshot({ path: path.join(outputDir, "painel_validacao_espacial_apos_limpeza.png"), fullPage: true });

    await page.getByText("Justificar pendências", { exact: true }).click();
    await page.locator("#just_tipo").waitFor({ state: "attached", timeout: 30000 });
    await page.waitForTimeout(1000);
    const bodyJust = await page.locator(".tab-pane.active").innerText();
    must(bodyJust.includes("Justificativas para pendências remanescentes"), "título de justificativas ausente");
    must(bodyJust.includes("A justificativa não corrige dados"), "aviso de não liberação de gates ausente");
    must(!bodyJust.includes("<span") && !bodyJust.includes("style=\"color"), "marcação HTML bruta visível na tabela de justificativas");
    must(await page.locator("#just_texto").isVisible(), "campo de justificativa ausente");
    await page.screenshot({ path: path.join(outputDir, "painel_justificativas.png"), fullPage: true });

    const browserErrorsBlocking = browserErrors.filter((message) =>
      !message.includes("status of 404") &&
      !message.includes("clearRect")
    );
    must(browserErrorsBlocking.length === 0, `erros bloqueantes no navegador: ${browserErrorsBlocking.join(" | ")}`);
    const result = {
      status: "PASS",
      url,
      stateBefore,
      stateAfter,
      checks: [
        "diagnóstico de formação vegetacional",
        "fluxo espacial unificado",
        "ausência do campo de linhas esperadas",
        "limpeza geral de filtros, coleta e coordenadas",
        "aba de justificativas auditáveis",
        "rótulos limpos na tabela de justificativas",
      ],
      browserErrors,
      browserErrorsBlocking,
    };
    fs.writeFileSync(path.join(outputDir, "resultado_painel_ui.json"), JSON.stringify(result, null, 2));
    process.stdout.write(`${JSON.stringify(result)}\n`);
  } finally {
    await browser.close();
  }
})().catch((error) => {
  process.stderr.write(`${error.stack || error}\n`);
  process.exitCode = 1;
});
