# Contrato de governança e desenvolvimento — Monitora-Campestre-Savanico

**Subtítulo:** Contrato-base consolidado com resultados das auditorias H2R/H2R-C e trilha 03.5I–03.5N.

**Premissa obrigatória:** o contrato-base anterior permanece vigente e integral; este documento consolida, não substitui. Nenhuma premissa consolidada pode ser violada sem aviso explícito e parada.

## 0. Cláusula de preservação do contrato-base

O contrato-base anterior permanece vigente, integral e aplicável a todo desenvolvimento futuro do Monitora-Campestre-Savanico.

Este documento é uma consolidação operacional e documental das premissas já pactuadas, dos resultados das auditorias H2R/H2R-C e da trilha incremental 03.5I–03.5N. Ele não revoga, não reduz e não substitui obrigações anteriores.

Qualquer tarefa que exija violar, enfraquecer ou contornar uma premissa deste contrato deve declarar a violação de forma explícita e parar antes de executar alterações.

## 1. Premissa-mãe: motor único de contrato

O projeto deve evoluir para um motor único de contrato sem comprometer o script validado.

O contrato único deve ser a fonte de verdade para importação, organização, pré-validação, painel, pós-validação, `registros_corrig.csv`, `registros_validados.csv`, estatísticas e gráficos.

A aplicação do contrato deve ocorrer por perfis derivados, e não por uma implementação monolítica que substitua todos os subsistemas de uma vez.

## 2. Baseline e estratégia

A referência técnica é a v2.6.0 golden baseline.

A `v2.6.2_teste` não deve ser remendada como base principal.

Funcionalidades posteriores à v2.6.0 devem ser reaplicadas em camadas pequenas, auditadas, performáticas e reversíveis.

O projeto não deve substituir subsistemas inteiros de uma vez. A estratégia correta é auditar fontes, criar uma fonte única embutida, criar índices e caches, e só então migrar consumidores.

## 3. Contrato único

O contrato único deve integrar, em uma única fonte de verdade:

- XLSForm 21FEV25.
- Template SISMONITORA com 129 atributos.
- Metadados administrativos.
- Colunas técnicas permitidas.
- Aliases e labels históricos, com e sem HTML.
- Paths.
- Names curtos.
- Tipos, `type_base` e `list_name`.
- Choices.
- `required`, `constraint` e `relevant`.
- Grupos, repeats e campo pai.
- Cardinalidade operacional.
- Mapeamento histórico -> canônico 2025.
- Severidade.
- Estágio.
- Origem da regra.
- Status de confiança.

Não há contratos concorrentes. Fontes paralelas, listas paralelas e hard-codes fora da fonte única são violações deste contrato.

## 4. Local e independência

A fonte única deve ser embutida no script público.

Não pode haver dependência runtime de XLSX, CSV, ZIP, planilhas externas, `output/` ou `log/`.

Fontes externas podem existir apenas para auditoria e desenvolvimento.

O template CSV 2025 integral não deve ser versionado se contiver dado real.

## 5. Perfis derivados

Os perfis operacionais devem ser derivados da fonte única, nunca mantidos como fontes paralelas:

- `perfil_importacao`.
- `perfil_pre_painel`.
- `perfil_painel_edicao`.
- `perfil_pos_painel_corrig`.
- `perfil_export_registros_validados`.
- `perfil_estatisticas_graficos`.

Cada perfil deve declarar seu estágio, escopo, campos aplicáveis, severidade, cardinalidade, regras relevantes e status de confiança.

## 6. Linhagem e escopo contrato-base

`registros_importados_bruto.csv` é a camada fiel/técnica de importação bruta.

`registros_importados.csv` é a camada canônica de importação, organizada e comparável, sem sanitização final.

A base pré-painel deve ser derivada por `perfil_pre_painel`.

`registros_corrig.csv` é camada operacional em correção, não necessariamente final.

`registros_validados.csv` é a projeção final SISMONITORA com 129 colunas, criada apenas quando a pós-validação permitir.

## 7. Linhagem consolidada pós-H2R/H2R-C

A cadeia materializada consolidada é:

`registros_importados_bruto.csv -> registros_importados.csv -> registros_importados_operacional_pre_painel.csv -> registros_corrig.csv -> registros_validados.csv`.

O novo produto é uma camada operacional pós-tokenização/pré-painel. Ele não substitui `registros_importados.csv`, `registros_corrig.csv` nem `registros_validados.csv`.

Esse produto precisa ter identidade própria:

- Catálogo.
- Produtos centrais.
- Localização em `output/01_produtos_dados`.
- Índice.
- Auditoria final.
- README, manual e documentação de output.
- Linhagem.
- Comparação com `registros_importados.csv` quando aplicável.

## 8. Semântica híbrida proibida

Nenhum arquivo pode carregar duas semânticas.

Se a camada muda, o nome muda.

É proibido sobrescrever `registros_importados.csv` com pós-tokenização.

É proibido reconstruir `registros_importados.csv` tardiamente a partir de `registros_corrig.csv`.

É proibido criar produto novo invisível fora do catálogo, índice e auditoria.

## 9. Pré-validação, painel e diagnósticos

A pré-validação é obrigatória.

O painel deve mostrar ocorrências, contagens, coletas, atributos, severidade, regra, origem, efeito e bloqueio de `registros_validados.csv`.

Pendências impeditivas sanadas tornam `registros_corrig.csv` validável.

Se pendências impeditivas persistirem, `registros_corrig.csv` pode existir em correção, mas `registros_validados.csv` não pode ser criado como produto validado.

## 10. Pós-validação/exportação

A pós-validação usa `perfil_pos_painel_corrig`.

Ela deve validar formatos, domínios, choices, `select_one`, `select_multiple`, obrigatórios, constraints, relevance, vínculos, cardinalidade, Encostam, `solo_nu`, metadados e estrutura SISMONITORA.

A exportação usa o resultado da pós-validação e assertivas baratas, sem revalidação pesada.

## 11. Estatísticas/gráficos

`registros_validados.csv`, quando criado, é a base preferencial para estatísticas e gráficos.

Se `registros_validados.csv` não existir, o fluxo deve respeitar o status de validação e não tratar base pendente como validada.

## 12. solo_nu/Encostam

`solo_nu` só é final.

Se `solo_nu` coexistir com Encostam, `solo_nu` deve ser removido.

Se Encostam estiver vazio, `solo_nu` pode ser inserido apenas na etapa adequada.

A regra só pode atuar em pós-painel/sanitizações. Ela nunca deve atuar em bruto, importados ou pré-painel escondendo pendências.

## 13. Cardinalidade/pipes

As categorias operacionais de cardinalidade são:

- `texto_livre`.
- `estruturado_completo_por_ponto`.
- `estruturado_condicional_esparso`.
- `select_multiple`.
- `tecnico_midia`.
- `fora_do_contrato`.
- `ambiguo_indeterminado`.

Texto livre preserva pipe.

Estruturado completo resolve por ponto absoluto.

Condicional/esparso resolve por elegibilidade e `relevance`.

`select_multiple` deve ser tratado como conjunto.

Ambíguo não deve ser forçado.

O caso bromélia prova que `select_one` condicional/esparso não pode resolver por ponto absoluto.

## 14. ea/ua

O contrato deve registrar `ea <-> estacao_amostral` e `ua <-> unidade_amostral` como requisito estrutural.

Esses vínculos são parte da estrutura contratual e não devem depender de inferência tardia ou mapeamento paralelo.

## 15. Performance

A implementação deve priorizar:

- `data.table`.
- Atualização por referência.
- Ausência de loops linha a linha quando houver alternativa vetorizada.
- Ausência de recomputação global desnecessária.
- Ausência de fuzzy repetido.
- Ausência de parse externo em runtime.
- Derivação do contrato e dos índices uma única vez.
- Operação por blocos.
- Registro de tempo quando houver risco.

Qualquer alteração com risco de piora de performance deve ser auditada antes de aprovação.

## 16. Índices/caches

Índices e caches devem ser sempre derivados da fonte única.

Devem existir índices/caches por:

- Path.
- Name.
- Label normalizado.
- Atributo canônico.
- Relevance e campo pai.
- `list_name` e choices.
- Aliases.
- Estágio.
- Severidade.
- Cardinalidade.

Nenhum índice ou cache pode virar fonte concorrente.

## 17. Auditoria/linhagem

Auditorias não sobrescrevem histórico.

`auditoria_registros_importados_resumo.csv` não pode apagar camada anterior.

O projeto pode aceitar resumo acumulativo ou por produto, camada e contexto, desde que a linhagem permaneça rastreável.

Auditorias devem registrar origem, destino, objeto fonte, transformação, produto, timestamp e status de contrato.

## 18. Testes

Os testes devem ser proporcionais ao risco:

- 03.5I: sem dataset real.
- 03.5J: sem dataset real.
- 03.5K: um dataset representativo diagnóstico.
- 03.5L: PNB golden + FNCS + dataset médio até importados.
- 03.5M: FNCS + sintético pipes.
- 03.5N: PNB + FNCS + painel.

H2R/H2R-C exige auditoria estática, teste leve/parse quando seguro e run representativa antes de aprovar.

Testes longos, uso de dados reais, pipeline PNB/FNCS e execuções pesadas exigem autorização explícita quando não estiverem no escopo da tarefa.

## 19. Critérios de parada

A execução deve parar diante de:

- Violação de premissa.
- Dado real sem autorização.
- Teste longo fora do combinado.
- Alteração não explicada em schema ou contagens.
- Novo pipe residual.
- Regressão Encostam, `solo_nu` ou `registros_validados.csv`.
- Piora de performance.
- Uso de `output/` ou `log/` como entrada.
- Risco de versionar dado real.
- Push não autorizado.
- Semântica híbrida.
- Auditoria sobrescrita.
- Produto invisível.
- Reconstrução tardia.
- Fonte ou lista paralela.
- `registros_corrig` tratado como final.

## 20. Segurança de dados/Git

Não versionar dados reais ou pessoais.

Não commitar CSV, ZIP, XLSX, XLS, ODS, RDS, RDA, SQLITE, DB, GPKG, SHP, GEOJSON, KML ou KMZ com dados reais.

Usar `git add` seletivo.

Nunca usar `git add .`.

Checar extensões staged antes de commit.

Relatórios podem conter nomes, paths, tipos, contagens, decisões, status e critérios. Relatórios nunca devem conter amostras reais sensíveis.

## 21. Plano incremental

03.5I: criar fonte única embutida sem pipeline.

03.5J: criar índices e caches.

03.5K: criar mapa observado -> canônico diagnóstico.

03.5L: consolidar `registros_importados.csv`.

03.5M: implementar resolvedor de pipes por `relevance` e cardinalidade.

03.5N: migrar painel, `registros_validados.csv` e relatórios.

## 22. Critérios de aceitação

O fluxo é aceitável quando:

- Bruto permanece fiel.
- Importado é canônico e comparável.
- Operacional pré-painel é governado.
- Históricos convergem para canônicos 2025.
- Textuais preservam pipes.
- Estruturados respeitam cardinalidade correta.
- Condicionais seguem `relevance`.
- Ambíguos não são forçados.
- Pendências chegam ao painel.
- `solo_nu` atua só pós-painel.
- `registros_corrig.csv` é validável.
- `registros_validados.csv` é fiel.
- Não há revalidação pesada na exportação.
- Performance é preservada.
- PNB/FNCS não regridem.
- Auditorias rastreiam decisões.

## 23. Regras Claude/Codex

Prompts devem começar com CHECKLIST DO AGENTE.

O agente deve economizar execução, priorizar auditoria de código, implementação cirúrgica, testes sintéticos pequenos e relatório.

O agente não deve rodar testes longos nem usar dados reais sem autorização.

O agente não deve ampliar escopo.

O agente não deve criar fonte paralela.

O agente não deve criar hard-codes fora da fonte única.

Auditoria não vira refatoração.

Sem push.

Sem `git add .`.

Sempre executar `git status --short` quando aplicável.

Confirmar commit/push antes de qualquer operação dessa natureza.

Relatórios devem ficar em `diagnostics/`.

## 24. Regra final

Este documento é base para todo desenvolvimento futuro.

Toda tarefa deve declarar conformidade com este contrato e parar se exigir violação.
