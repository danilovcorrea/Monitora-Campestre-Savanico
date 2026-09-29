# Revisão documental r09 — PNM — 29/09/2026

Status: aguardando avaliação do usuário. Demais gates e publicação não executados.

Alterações: centralização por coluna dos valores quantitativos e respectivos cabeçalhos em HTML/PDF/DOCX; glossário organizado em tabelas temáticas, com ordenação alfabética interna; definições de siglas integradas ao primeiro uso, reconhecimento de definições existentes e compartilhamento entre singular/plural; notas vinculadas às tabelas/figuras quando o primeiro uso ocorre nesses elementos; eliminação de quebras acumuladas antes do índice; identificação atualizada nas capas e no rodapé textual.

O glossário mantém significados distintos para PA (ponto amostral) e PA (localizador: Pará). Grupos sem entradas são omitidos; unidades/símbolos são ordenados pelo significado por extenso. Figuras e conteúdo científico não foram recalculados.

A rotina reproduzível `tools/v305/regerar_documentos.R` recebe repositório, destino e origem. O destino deve conter previamente os recursos e resultados existentes. Os documentos de origem são preservados. A capa do Word é regenerada com os mesmos metadados do texto, pois é uma imagem, não um parágrafo editável.

Conferência focal: testes de transformação editorial; comparação das matrizes científicas e hashes das figuras com r08; igualdade de conteúdo HTML/DOCX; índice no Word após salvar/reabrir; páginas vazias, ignorando rodapés; alinhamento dos valores/cabeçalhos; revisão visual dirigida. O teste de páginas vazias rejeita corretamente o PDF detalhado r08, identificando as páginas 2–6. Estas verificações não equivalem à homologação integral da candidata.

Preservados em PNM: 31 tabelas científicas no detalhado, 7 no sintético e 56 arquivos de figuras. Foram regenerados somente os relatórios analíticos de PNM nos cinco formatos existentes. FNB e PNI não foram reprocessados.

Evidências locais: `/home/dlinux/Monitora_Dev_20260712/revisao_pnm_v305_r09_20260929`. Entrega prevista: `PNM/v305_r09` no diretório de dados pré-validados do projeto.

Script r09: 4.896.601 bytes LF e 4.980.827 bytes CRLF, abaixo de 5.000.000 bytes.
