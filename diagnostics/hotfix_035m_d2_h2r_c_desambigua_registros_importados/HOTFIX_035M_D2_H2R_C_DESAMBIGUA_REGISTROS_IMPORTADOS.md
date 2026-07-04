# Hotfix 03.5M-D2-H2R-C — Desambiguação da segunda escrita de `registros_importados.csv`

## 1. Resumo executivo

Aplicado o patch cirúrgico recomendado pela auditoria H2R: a segunda escrita (pós-tokenização) que antes sobrescrevia `registros_importados.csv` a partir de `registros_corrig` agora grava um produto com identidade própria — `registros_importados_operacional_pre_painel.csv`. A escrita canônica de `registros_importados.csv` (a partir de `registros`, pré-consolidação) permanece exatamente como estava, nunca mais sobrescrita. A mudança foi feita adicionando um único parâmetro opcional (`produto_nome`) ao exportador já existente, sem duplicar nenhuma lógica de tokenização/resolução de pipe.

## 2. Achado H2R que motivou o patch

A auditoria 03.5M-D2-H2R confirmou que `registros_importados.csv` era escrito duas vezes com semânticas diferentes sob o mesmo nome: uma vez a partir de `registros` (pré-consolidação, resíduo de pipe só informativo, "produto comparável usado na curadoria por bolsista"), e depois sobrescrito a partir de `registros_corrig` (pós-tokenização, resíduo de pipe bloqueante). O próprio código já documentava essa sobrescrita como intencional (comentário "Produto operacional de importação: **sobrescreve** registros_importados.csv..."), mas isso violava a premissa conceitual de que produtos de camadas diferentes devem ter identidade própria.

## 3. Alteração aplicada

**Função alterada** (definição viva, linha ~33572 antes do patch — a definição morta/sobrescrita em ~24631, nunca usada por nenhuma chamada do script, não foi tocada): `monitora_registros_importados_saneado_exportar()`.

Adicionado o parâmetro opcional:
```r
produto_nome = "registros_importados.csv"
```

Usado internamente em todos os pontos que antes tinham o nome do produto hardcoded como string literal:
- `produto = produto_nome` na chamada a `monitora_produtos_resolver_pipes_por_ponto()` (só o argumento passado mudou; a função em si não foi alterada).
- `produto = produto_nome` na chamada a `monitora_bloquear_pipe_residual_produto()` (idem).
- Caminho de escrita: `monitora_produtos_escrever_csv_canonico(out, produto_nome, ...)` e `monitora_produtos_path_raiz(produto_nome, ...)`.
- Resumo/auditoria: coluna `produto` do `data.table` de resumo agora usa `produto_nome`.
- Mensagens de log/erro: usam `produto_nome` em vez do texto fixo "registros_importados.csv".

**Chamadas existentes que não informam `produto_nome` continuam com o comportamento exatamente inalterado** (default `"registros_importados.csv"`).

**Ponto de integração alterado** (única chamada que passou a usar o parâmetro novo): o bloco de exportação pós-tokenização (linha ~38851 antes do patch), que agora chama:
```r
monitora_registros_importados_saneado_exportar(
  registros_corrig, ...,
  contexto = "pre_painel_pos_extracao_tokens_operacionais",
  bloquear_pipe_residual = TRUE,
  produto_nome = "registros_importados_operacional_pre_painel.csv"
)
```

O comentário do bloco foi reescrito para deixar explícito que `registros_importados.csv` permanece camada canônica e não é mais sobrescrito por este ponto.

## 4. Funções/blocos alterados

- `monitora_registros_importados_saneado_exportar()` — definição viva (a única realmente chamada pelo script).
- O bloco de chamada pós-tokenização (comentário + argumentos da chamada + mensagens de `stop()`/`monitora_perf_registrar_checkpoint()`).

**Nenhuma outra função foi alterada.** A primeira chamada (escrita canônica, a partir de `registros`) não recebeu `produto_nome` — permanece usando o default, sem qualquer mudança de comportamento.

## 5. Nome do novo produto operacional

`registros_importados_operacional_pre_painel.csv`, conforme a preferência inicial indicada no checklist.

## 6. Confirmação: `registros_importados.csv` deixou de ser sobrescrito pela segunda escrita

Confirmado — a segunda chamada agora usa `produto_nome = "registros_importados_operacional_pre_painel.csv"`, um arquivo diferente. Testado explicitamente (seção 10, teste 2): após a segunda chamada, o arquivo `registros_importados.csv` da primeira chamada permanece com `mtime` idêntico (não reescrito).

## 7. Confirmação: a primeira escrita de `registros_importados.csv` foi preservada

Confirmado — a chamada da primeira escrita (linha ~34613, a partir de `registros`, contexto `"pos_consolidacao_aliases_importacao_pre_registros_corrig"`) não foi alterada em nenhum caractere; `git diff` não mostra nenhuma mudança nessa região do código.

## 8. Confirmação: `registros_importados_bruto.csv` foi preservado

Confirmado — nenhuma alteração em `monitora_publicacao_h_fwrite_registros_importados()` nem em sua única chamada (linha ~34549); não faz parte do escopo deste patch.

## 9. Confirmação de áreas restritas

```
$ git diff -- monitora_campsav_alvo_global_v2.6.0.R | grep -E 'word\(|padronizacao_formas_vida_condicionais_basicas|monitora_produtos_resolver_pipes_por_ponto|monitora_bloquear_pipe_residual_produto|monitora_produtos_classificar_pipe_coluna'
     if (exists("monitora_produtos_resolver_pipes_por_ponto", mode = "function")) {
       out <- monitora_produtos_resolver_pipes_por_ponto(
     if (exists("monitora_bloquear_pipe_residual_produto", mode = "function")) {
       monitora_bloquear_pipe_residual_produto(
 ### padronizacao_formas_vida_condicionais_basicas, padronizacao_formas_vida_outras,
```

As 4 primeiras ocorrências são apenas o **argumento passado** (`produto = produto_nome` em vez de `produto = "registros_importados.csv"`) dentro de chamadas já existentes a essas 2 funções — nenhuma delas foi editada; suas definições permanecem byte-a-byte idênticas. A última ocorrência é só texto de comentário atualizado (menciona o nome do bloco de normalização para dar contexto), sem nenhuma alteração de código nessa área.

- **Motor único**: não alterado — nenhuma função de contrato único tocada.
- **Painel**: não alterado — o painel consome `registros_corrig` em memória, nunca releu `registros_importados.csv` do disco (confirmado na auditoria H2R).
- **Validações**: `registros_validados.csv` e seu gate de pendências não foram tocados.
- **`word(...,-1)`/normalização condicional/resolvedores de pipe**: nenhuma linha alterada, só o argumento `produto=` já discutido acima.
- **Schemas**: nenhuma coluna nova adicionada a nenhum produto; os dois arquivos gerados têm exatamente as mesmas colunas do objeto de entrada.

## 10. Testes realizados

Extração verbatim da definição **viva** (última ocorrência no script, já que há uma definição anterior morta/sobrescrita nunca chamada) para um harness isolado, com *stand-ins* leves só para as 2 funções de pipe (já testadas exaustivamente em etapas anteriores) e para 2 dependências auxiliares não relacionadas à mudança (`monitora_sanitizar_ausencias_produto`, `monitora_produtos_copiar_canonico_para_raiz`).

| # | Teste | Resultado |
|---|---|---|
| 1 | Chamada sem `produto_nome` | Retorno `TRUE`; escreve `registros_importados.csv`; produto recebido pelo bloqueio de pipe = `"registros_importados.csv"` |
| 2 | Chamada com `produto_nome = "registros_importados_operacional_pre_painel.csv"` | Retorno `TRUE`; escreve o novo arquivo; produto recebido pelo bloqueio de pipe = novo nome; `registros_importados.csv` do teste 1 **não foi sobrescrito** (`mtime` idêntico) |
| 3 | Confirmar que `registros_validados.csv` nunca é criado | Confirmado ausente |
| 4 | Confirmar que nenhum dos 2 produtos ganhou coluna extra | Confirmado — colunas idênticas ao objeto de entrada em ambos |
| 5 | Confirmar que o resumo de auditoria reflete o `produto_nome`/`contexto` corretos | Confirmado |

**Parse**: `Rscript -e 'invisible(parse(...)); cat("PARSE_OK\n")'` → `PARSE_OK`, tanto no arquivo completo quanto no arquivo isolado extraído.

## 11. Riscos remanescentes

- **Compatibilidade externa**: usuários/scripts externos que hoje esperam encontrar em `registros_importados.csv` o conteúdo **pós-tokenização** (a versão que antes sobrescrevia) passarão a encontrar só a versão **pré-consolidação**. Este risco já foi identificado na auditoria H2R (seção 17) e não é resolvido por este patch isoladamente — recomenda-se comunicação/changelog antes de uma run real de produção usar esta mudança.
- **Diagnóstico de contrato único (03.5L-B)**: continua rodando só sobre a versão pré-consolidação de `registros` (checkpoint1) e sobre `registros_corrig` pós-tokenização (checkpoint2, opera sobre o objeto, não sobre o produto renomeado) — nenhuma mudança de comportamento aqui, mas vale registrar que essa assimetria (já preexistente, notada na H2R) não foi resolvida por este patch, que ficou estritamente no escopo do nome do arquivo.
- **Definição morta duplicada**: a versão anterior/morta de `monitora_registros_importados_saneado_exportar()` (nunca chamada) permanece sem o parâmetro novo — não representa risco operacional (nunca é invocada), mas fica como débito técnico de limpeza para uma etapa futura, se desejado.

## 12. Recomendação para próxima etapa

Antes de qualquer run real: (a) atualizar documentação/README que descreva `registros_importados.csv` para deixar claro que ele agora reflete só a camada pré-consolidação; (b) considerar uma etapa H2R-E (conforme plano da H2R) para revisar se algum consumidor **externo** (fora do script, não mapeado nesta auditoria) depende do conteúdo antigo pós-tokenização; (c) rodar uma FNCS real controlada, com console log preservado, para confirmar que ambos os arquivos (`registros_importados.csv` e `registros_importados_operacional_pre_painel.csv`) aparecem corretamente povoados e distintos em `output/01_produtos_dados/`.

## 12.1. Nota de integração H2R-C

Hotfix complementar local, sem commit: `registros_importados_operacional_pre_painel.csv` foi integrado à auditoria final de produtos esperados, às listas internas de produtos centrais/`01_produtos_dados`, à descrição textual rastreada de produtos de dados, ao texto gerado do manual e ao README. A inclusão mantém o arquivo como camada operacional pós-tokenização/pré-painel e explicita que ele não substitui `registros_importados.csv`, `registros_corrig.csv` nem `registros_validados.csv`.

Permanecem pendentes a validação representativa em execução controlada e a aprovação final H2R-C.

## 13. `git diff --stat`

```
$ git diff --stat
 monitora_campsav_alvo_global_v2.6.0.R | 53 ++++++++++++++++++++++-------------
 1 file changed, 34 insertions(+), 19 deletions(-)
```

## 14. `git status --short --branch`

```
## dev-v2.6.2-h2r-cadeia-produtos...origin/dev-v2.6.2-h2r-cadeia-produtos
 M monitora_campsav_alvo_global_v2.6.0.R
?? diagnostics/hotfix_035m_d2_h2r_c_desambigua_registros_importados/
?? monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R
```

O arquivo `monitora_campsav_alvo_global_v2.6.0_03.5L-C_PNB_FLAG_OFF.R` já estava presente (untracked) antes desta tarefa — não foi criado nem alterado.

Nesta tarefa, nenhum `git add`, commit ou push foi executado.
