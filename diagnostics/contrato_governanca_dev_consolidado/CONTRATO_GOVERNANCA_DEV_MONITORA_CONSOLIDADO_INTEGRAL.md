# Contrato de governança e desenvolvimento — Monitora-Campestre-Savanico

**Subtítulo:** Versão integral consolidada, sem supressão de premissas e diretrizes.

**Natureza do documento:** este documento substitui a versão consolidada resumida anterior como base de desenvolvimento documental. Ele não substitui o contrato-base original: preserva, explicita e consolida suas premissas, seções e diretrizes, incorporando as atualizações H2R/H2R-C e a linhagem de produtos sem supressão.

**Rastreabilidade:** a versão consolidada resumida anterior permanece como registro histórico, mas não deve ser usada como contrato-base de desenvolvimento porque reduziu granularidade normativa. Esta versão integral deve ser usada para orientar auditorias, hotfixes documentais, evolução incremental e revisões futuras do motor único.

## 0. Cláusula de preservação do contrato-base e regra de não supressão

O contrato-base permanece vigente, integral e vinculante. Nenhum desenvolvimento, auditoria, hotfix, documentação derivada ou refatoração pode reduzir, apagar, rebaixar ou reinterpretar premissas do contrato-base por omissão.

Esta versão consolidada tem função de preservação e consolidação. Ela deve incorporar H2R/H2R-C e a linhagem de produtos, mas sem transformar o contrato em resumo. Se houver divergência entre uma síntese anterior e o contrato-base, prevalece o contrato-base; se esta versão detalhar uma regra já existente, o detalhamento deve ser tratado como especificação operacional da mesma regra.

Toda tarefa futura deve parar antes de executar alterações quando exigir qualquer uma das seguintes ações:

- violar premissa consolidada;
- substituir uma seção integral por resumo;
- criar produto com semântica ambígua;
- usar `output/` ou `log/` como fonte de verdade;
- reconstruir tardiamente produto de camada anterior;
- alterar código, rodar dados reais, pipeline pesado, staging, commit, push, reset, clean ou sudo sem autorização literal no escopo da tarefa.

## 1. Premissa-mãe

O Monitora-Campestre-Savanico deve evoluir para um motor único de contrato, mantendo a baseline validada e evitando substituições amplas e simultâneas de subsistemas. A fonte contratual deve ser única, embutida e capaz de derivar todos os perfis operacionais necessários ao fluxo.

O contrato único deve orientar importação, organização, pré-validação, painel, pós-validação, correção, exportação, estatísticas, gráficos, auditoria e linhagem. A implantação deve ocorrer por camadas pequenas, auditáveis, reversíveis e compatíveis com a linha golden.

Nenhuma regra local, lista paralela, hard-code ou cache independente pode virar fonte concorrente. Índices, caches e produtos auxiliares podem existir apenas como derivados rastreáveis do contrato único.

## 2. Baseline e estratégia de evolução

A referência técnica de estabilidade é a v2.6.0 golden baseline. A evolução deve reaplicar funcionalidades posteriores por etapas pequenas, auditadas e performáticas, sem remendar uma base de teste como se fosse a base principal.

A estratégia correta é:

- auditar a fonte atual e seus consumidores;
- embutir a fonte única no script público;
- criar índices e caches derivados;
- migrar consumidores gradualmente;
- validar cada etapa com testes proporcionais ao risco;
- preservar capacidade de comparar comportamento anterior e posterior;
- evitar mudanças simultâneas em importação, painel, validação e exportação.

Qualquer alteração com risco de modificar schema, contagem de linhas, semântica de produto, cardinalidade, performance ou bloqueios de validação deve ser tratada como intervenção auditável.

## 3. Conceito de contrato único

O contrato único é a fonte normativa que integra, no mínimo:

- XLSForm 21FEV25;
- template SISMONITORA de 129 atributos;
- metadados administrativos;
- colunas técnicas permitidas por estágio;
- paths;
- names curtos;
- labels históricos, com e sem HTML;
- aliases;
- tipos, `type_base` e `list_name`;
- choices;
- `required`, `constraint` e `relevant`;
- grupos, repeats e campo pai;
- cardinalidade operacional;
- vínculo observado -> canônico 2025;
- severidade;
- estágio aplicável;
- origem da regra;
- status de confiança.

O contrato único não é um relatório e não é um produto de saída. Ele é a matriz normativa da qual derivam perfis, índices, caches, diagnósticos e validações. Fontes paralelas podem existir apenas como material de auditoria ou desenvolvimento, nunca como dependência runtime.

## 4. Local e independência do contrato

A fonte única deve estar embutida no script público, sem depender em runtime de XLSX, CSV, ZIP, planilhas externas, `output/`, `log/` ou artefatos gerados em execuções anteriores.

Arquivos externos podem ser usados para auditoria, desenvolvimento ou reconstrução documentada da fonte embutida, desde que não contenham dados reais sensíveis e não sejam necessários para a execução normal. O template CSV 2025 integral não deve ser versionado quando houver risco de conter dado real.

O contrato embutido deve permitir execução reprodutível: a ausência de artefatos de saída anteriores não pode mudar a interpretação contratual.

## 5. Perfis derivados do contrato único

Os perfis operacionais devem ser derivados do contrato único e não mantidos como fontes paralelas. Cada perfil deve declarar estágio, campos aplicáveis, severidade, cardinalidade, regras relevantes, status de confiança e escopo de atuação.

Perfis mínimos:

- `perfil_importacao`;
- `perfil_pre_painel`;
- `perfil_painel_edicao`;
- `perfil_pos_painel_corrig`;
- `perfil_export_registros_validados`;
- `perfil_estatisticas_graficos`.

Um perfil pode filtrar ou aplicar regras do contrato conforme estágio, mas não pode redefinir significado de campo, cardinalidade ou domínio. Se uma regra ainda não tiver confiança suficiente para bloquear, ela deve ser marcada como diagnóstica, assistida ou informativa.

## 6. Perfil de importação

O perfil de importação deve atuar sobre leitura e reconhecimento das fontes observadas. Seu objetivo é mapear o observado para o canônico sem aplicar sanitização semântica final, correção ecológica, painel, `solo_nu` final ou regras pós-painel.

O perfil deve cobrir:

- reconhecimento de fontes por path, name, label e aliases;
- labels com HTML e labels sem HTML;
- diferenças entre nomes curtos, labels históricos e nomes canônicos;
- sufixos de duplicidade como `__dup`, `.1`, `.2` e variações equivalentes;
- mapeamento observado -> canônico;
- identificação de colunas ambíguas;
- identificação de colunas fora do contrato;
- preservação de campos não resolvidos quando a resolução não for segura;
- diagnóstico de cardinalidade e pipes sem forçar conversão indevida.

Campos ambíguos devem permanecer marcados como ambíguos até que o contrato ou uma intervenção assistida resolva a correspondência. O perfil de importação pode organizar, consolidar aliases e produzir comparabilidade, mas não pode mascarar pendências que pertençam ao pré-painel, painel ou pós-validação.

## 7. registros_importados_bruto.csv

`registros_importados_bruto.csv` é a camada fiel/técnica da importação bruta. Ele representa o snapshot inicial materializado após leitura, concatenação e tratamento técnico estritamente necessário para gravar o produto.

Este produto deve preservar nomes originais observáveis e servir à rastreabilidade da importação. Ele não deve receber sanitização semântica final, correções ecológicas, decisões de painel, normalizações condicionais tardias ou reconstrução por produto posterior.

Se `registros_importados_bruto.csv` não for materializado no ponto correto do fluxo, ele não deve ser recriado no fim a partir de `registros_corrig.csv`, `registros_validados.csv` ou outro produto derivado. A reconstrução tardia de camada anterior é violação de linhagem.

## 8. registros_importados.csv

`registros_importados.csv` é a camada canônica de importação, organizada e comparável, derivada do objeto de importação antes da transformação operacional pós-tokenização. Ele deve refletir importação saneada por contrato de importação, aliases e estrutura observada, sem ser transformado em base pós-painel, checkpoint operacional ou produto final validado.

É proibido sobrescrever `registros_importados.csv` com conteúdo pós-tokenização ou com conteúdo derivado de `registros_corrig.csv`. Também é proibido reconstruir `registros_importados.csv` tardiamente a partir de `registros_corrig.csv`, porque isso apagaria a camada original e criaria semântica híbrida.

O produto deve ser comparável com camadas posteriores quando aplicável, especialmente com `registros_importados_operacional_pre_painel.csv`, mas a comparação não altera sua identidade. Ele pode ter auditorias próprias, desde que a auditoria preserve histórico por produto, camada e contexto.

## 9. Base pré-materializada para painel

A base pré-materializada para painel deve ser derivada de `registros_importados.csv` por meio do contrato único e do `perfil_pre_painel`. Sua função é preparar diagnóstico operacional para o painel sem substituir a camada de importação e sem declarar validação final.

Ela deve identificar, de forma rastreável:

- ocorrências diagnósticas;
- pontos afetados;
- linhas e coletas afetadas;
- atributos envolvidos;
- severidade;
- regra violada;
- origem da regra;
- efeito esperado;
- bloqueio ou não de `registros_validados.csv`;
- tipo de correção esperado: manual, assistida ou automática pós-painel.

A materialização pós-H2R/H2R-C reconhece `registros_importados_operacional_pre_painel.csv` como camada operacional pós-tokenização/pré-painel. Esse arquivo não substitui `registros_importados.csv`, `registros_corrig.csv` nem `registros_validados.csv`; ele deve ter identidade própria em catálogo, produtos centrais, `output/01_produtos_dados`, índice de produtos, auditoria final, README, manual, documentação de output, linhagem e comparação com `registros_importados.csv` quando aplicável.

## 10. Perfil de pré-validação pré-painel

O perfil de pré-validação pré-painel é obrigatório. Ele deve identificar inconsistências que impediriam:

- validação futura de `registros_corrig.csv`;
- criação segura de `registros_validados.csv`;
- equivalência SISMONITORA;
- importação posterior em sistemas dependentes;
- abertura do painel sem perda de contexto relevante.

Essa pré-validação deve ocorrer antes do painel e deve ser diagnóstica, explícita e rastreável. Ela não deve corrigir silenciosamente o que precisa ser visto pelo bolsista, curador ou operador.

Pendências impeditivas devem bloquear produto final validado. Pendências não impeditivas podem ser registradas como alertas, desde que seu status e severidade fiquem claros.

## 11. Painel de correções

O painel de correções deve apresentar as ocorrências de modo operacionalmente útil. No mínimo, deve exibir:

- ocorrência;
- contagem;
- coleta;
- ponto;
- linha;
- atributo;
- valor observado;
- severidade;
- regra violada;
- origem da regra;
- efeito esperado da correção;
- status de bloqueio;
- tipo de correção: manual, assistida ou automática pós-painel.

O painel não pode ocultar pendências impeditivas nem converter diagnóstico em validação. Ele opera sobre camada de correção/checkpoint e deve preservar o vínculo entre alteração feita, regra contratual e consequência esperada.

Quando uma correção for assistida, a regra aplicada deve ser rastreável. Quando for manual, o resultado deve ser passível de pós-validação. Quando for automática, deve ocorrer no estágio autorizado pelo contrato.

## 12. Escopo das ocorrências diagnósticas

As ocorrências diagnósticas devem ser suficientes para orientar correção e determinar bloqueio. Se o bolsista ou operador sanar todas as pendências impeditivas, `registros_corrig.csv` deve ficar validável pelo perfil pós-painel.

Se pendências impeditivas persistirem, `registros_corrig.csv` pode existir como camada operacional em correção ou checkpoint auditável, mas `registros_validados.csv` não pode ser criado como produto validado.

Ocorrências diagnósticas não devem ser confundidas com erro fatal de importação. Elas podem ser informativas, impeditivas, assistidas, dependentes de revisão ou estruturais. A severidade deve ser derivada do contrato único e do estágio aplicável.

## 13. registros_corrig.csv

`registros_corrig.csv` é camada operacional em correção e checkpoint auditável. Ele pode refletir correções manuais, correções assistidas, sanitizações permitidas e normalizações autorizadas no estágio correspondente.

Esse produto não é, por si só, produto final validado. Ele pode conter pendências impeditivas ou não impeditivas, e seu status deve ser determinado por auditoria de pendências e pós-validação.

`registros_corrig.csv` pode ser reexportado em checkpoints conforme o fluxo operacional, desde que não seja usado para reconstruir `registros_importados.csv` nem para apagar histórico de camadas anteriores. Ele é fonte para validação final apenas quando as pendências impeditivas forem sanadas.

## 14. Perfil de pós-validação pós-painel

O perfil de pós-validação pós-painel deve atuar depois das correções manuais, assistidas e sanitizações autorizadas. Ele valida se `registros_corrig.csv` está apto a gerar `registros_validados.csv`.

Esse perfil deve validar, no mínimo:

- formatos;
- domínios;
- choices;
- `select_one`;
- `select_multiple`;
- obrigatórios;
- constraints;
- relevance;
- vínculos;
- cardinalidade;
- regra de Encostam;
- regra de `solo_nu`;
- metadados administrativos;
- estrutura SISMONITORA;
- ausência de pendências impeditivas.

A pós-validação não deve depender de artefatos anteriores em disco. Ela deve usar o objeto/camada em correção e o contrato único, com índices derivados quando necessário.

## 15. registros_validados.csv

`registros_validados.csv` é a projeção final contratual SISMONITORA quando ativada e apta. Ele deve existir apenas quando a pós-validação indicar ausência de pendências impeditivas e quando a opção de geração estiver ativada.

O produto deve respeitar o schema contratual de 129 colunas, nomes, ordem, domínios e estrutura esperada. Não deve conter colunas técnicas, status, selos ou metadados auxiliares dentro do CSV principal quando isso violar a estrutura SISMONITORA.

Se houver pendências impeditivas, a geração deve ser bloqueada. Nesse caso, a existência de `registros_corrig.csv` não autoriza tratar a base como validada.

## 16. Perfil de exportação de registros_validados.csv

O perfil de exportação de `registros_validados.csv` não deve reexecutar validação pesada. Ele deve usar o resultado da pós-validação e aplicar assertivas baratas e finais.

Assertivas mínimas:

- status apto para exportação;
- ausência de pendências impeditivas;
- projeção para as 129 colunas SISMONITORA;
- nomes e ordem de colunas corretos;
- ausência de colunas técnicas indevidas;
- assinatura/schema compatível;
- relatório de exportação e bloqueios.

A exportação é etapa de projeção e publicação controlada, não uma nova etapa de correção. Se uma falha estrutural aparecer aqui, o fluxo deve retornar ao perfil de pós-validação ou parar, não remendar silenciosamente o produto final.

## 17. Estatísticas e gráficos

Estatísticas e gráficos devem preferir `registros_validados.csv` quando ele existir e estiver apto. Quando o produto final não existir, fluxos diagnósticos podem usar `registros_corrig.csv` ou camadas operacionais, desde que o status não seja apresentado como validado.

O perfil de estatísticas e gráficos deve declarar sua fonte, estágio, filtros, limitações e status contratual. Métricas derivadas de camada em correção devem carregar essa condição.

Nenhum gráfico ou estatística pode induzir que uma base pendente, pré-painel, importada ou operacional equivale ao produto final SISMONITORA.

## 18. Regra de solo_nu e Encostam

`solo_nu` é regra final e só pode atuar no estágio adequado, após painel/sanitizações e dentro da pós-validação ou preparação final autorizada. A regra não deve atuar em bruto, importados ou pré-painel de forma a esconder pendências.

Se `solo_nu` coexistir com Encostam, `solo_nu` deve ser removido conforme a regra contratual. Se Encostam estiver vazio e a regra final exigir, `solo_nu` pode ser inserido apenas no estágio final autorizado.

A regra deve ser testada para não regredir `registros_validados.csv`, não criar colunas indevidas e não alterar camadas anteriores.

## 19. Cardinalidade e pipes

O contrato deve classificar cardinalidade operacional e orientar tratamento de pipes. Categorias mínimas:

- `texto_livre`;
- `estruturado_completo_por_ponto`;
- `estruturado_condicional_esparso`;
- `select_multiple`;
- `tecnico_midia`;
- `fora_do_contrato`;
- `ambiguo_indeterminado`.

Texto livre preserva pipe. Estruturado completo pode resolver por ponto absoluto quando a regra permitir. Estruturado condicional/esparso deve respeitar elegibilidade e `relevance`. `select_multiple` deve ser tratado como conjunto. Campos ambíguos não devem ser forçados.

O caso bromélia demonstra que `select_one` condicional/esparso não pode ser resolvido por ponto absoluto de forma genérica. Qualquer resolvedor de pipe deve ser derivado do contrato, registrado por produto e bloqueante apenas no estágio correto.

## 20. ea e ua

O contrato deve registrar `ea <-> estacao_amostral` e `ua <-> unidade_amostral` como vínculos estruturais. Esses vínculos são parte da interpretação canônica, não uma inferência tardia opcional.

Qualquer produto que precise desses vínculos deve recebê-los por perfil derivado do contrato único. O mapeamento não deve ser refeito por regra local paralela, e divergências devem ser diagnosticadas com severidade compatível.

## 21. Performance

A evolução deve preservar performance e evitar recomputações desnecessárias. Preferências técnicas:

- `data.table`;
- atualização por referência quando segura;
- vetorização em vez de loops linha a linha;
- derivação do contrato e índices uma única vez;
- ausência de fuzzy repetido;
- ausência de parse externo em runtime;
- operação por blocos;
- relatórios de tempo quando houver risco;
- não reler produtos de disco quando o objeto já está em memória.

É proibido resolver problema de linhagem reconstruindo tardiamente camada anterior a partir de produtos posteriores. Além de violar semântica, isso tende a piorar performance e mascarar o ponto correto de materialização.

## 22. Índices e caches derivados

Índices e caches devem ser derivados do contrato único e nunca se tornar fontes concorrentes. Eles devem poder ser reconstruídos a partir da fonte embutida.

Índices/caches mínimos:

- path;
- name;
- label normalizado;
- atributo canônico;
- aliases;
- relevance e campo pai;
- `list_name` e choices;
- estágio;
- severidade;
- cardinalidade;
- status de confiança.

Se um índice divergir da fonte única, o índice está errado. Caches podem acelerar execução, mas não podem alterar interpretação normativa.

## 23. Validações necessárias e suficientes

O fluxo deve aplicar exatamente as validações necessárias e suficientes por estágio:

- pré-validação antes do painel;
- pós-validação após painel, correções e sanitizações autorizadas;
- assertivas baratas na projeção/exportação de `registros_validados.csv`.

Não deve haver nova validação pesada na exportação final. A exportação deve confiar no resultado da pós-validação e apenas confirmar condições finais de schema, status e ausência de pendências impeditivas.

Validações repetidas só são aceitáveis se forem baratas, idempotentes, explicitamente justificadas e não criarem comportamento diferente por ordem de execução.

## 24. Testes

Testes devem ser proporcionais ao risco e ao estágio. A matriz mínima de referência é:

- 03.5I: sem dataset real;
- 03.5J: sem dataset real;
- 03.5K: dataset representativo diagnóstico;
- 03.5L: PNB golden + FNCS + dataset médio até importados;
- 03.5M: FNCS + sintético pipes;
- 03.5N: PNB + FNCS + painel.

H2R/H2R-C exige auditoria estática, teste leve/parse quando seguro e run representativa antes de aprovação final. Dados reais, pipeline PNB/FNCS, testes longos e execuções pesadas exigem autorização explícita quando não estiverem no escopo.

Testes de contrato devem cobrir, no mínimo, preservação de `registros_importados_bruto.csv`, não sobrescrita de `registros_importados.csv`, identidade de `registros_importados_operacional_pre_painel.csv`, bloqueio de `registros_validados.csv` com pendências impeditivas e ausência de colunas técnicas no produto final.

## 25. Critérios de parada

O trabalho deve parar diante de:

- violação de premissa;
- dado real sem autorização;
- pipeline pesado fora do escopo;
- alteração não explicada em schema, nomes ou ordem de colunas;
- alteração não explicada em contagens;
- novo pipe residual bloqueante;
- regressão em Encostam, `solo_nu` ou `registros_validados.csv`;
- piora de performance não auditada;
- uso de `output/` ou `log/` como entrada normativa;
- risco de versionar dado real;
- staging, commit, push, tag, release, reset, clean ou sudo sem autorização literal;
- hotfix que exija violar premissa consolidada.

Parar significa registrar o motivo, preservar artefatos diagnósticos e pedir decisão explícita antes de continuar.

## 26. Segurança de dados e Git

Dados reais não devem ser versionados nem usados sem autorização explícita. Artefatos de output e logs são produtos diagnósticos ou operacionais, não fontes de contrato.

Regras Git:

- não fazer staging sem autorização;
- não fazer commit sem `AUTORIZO_COMMIT`;
- não fazer push/tag/release sem `AUTORIZO_PUBLICAR_REMOTO`;
- não usar `reset --hard`, rebase destrutivo, clean destrutivo ou remoção ampla;
- não apagar backups, logs ou artefatos diagnósticos;
- não alterar arquivos fora do repositório;
- preservar mudanças preexistentes do usuário.

Toda intervenção deve começar com auditoria de branch, HEAD, status e diffstat.

## 27. Plano incremental do motor único

O motor único deve avançar por etapas:

- consolidar contrato embutido;
- derivar perfis;
- criar índices/caches;
- conectar diagnóstico opt-in;
- conectar importação;
- conectar pré-validação;
- conectar painel;
- conectar pós-validação;
- conectar exportação;
- conectar estatísticas e gráficos;
- remover fontes paralelas somente após equivalência comprovada.

Cada etapa deve ter critérios de aceite, testes proporcionais e rollback conceitual. H2R-C é transição aceitável apenas porque respeita o contrato-base ao separar semânticas de produto; não é substituto do motor único.

## 28. Critérios de aceitação do motor único

O motor único será aceitável quando:

- todos os perfis forem derivados da mesma fonte;
- importação reconhecer observado -> canônico sem fontes paralelas;
- pré-validação produzir diagnósticos suficientes;
- painel exibir regras, severidade, origem e bloqueio;
- pós-validação determinar aptidão de `registros_corrig.csv`;
- exportação gerar `registros_validados.csv` com 129 colunas quando apto;
- índices/caches forem deriváveis;
- produtos tiverem identidade e linhagem próprias;
- testes demonstrarem equivalência ou mudança explicitamente aprovada;
- performance permanecer aceitável;
- documentação, README, manual, índice e auditoria final refletirem a cadeia de produtos.

Produto novo invisível fora de catálogo, índice, README/manual/output e auditoria final é violação.

## 29. Regras para Claude/Codex

Agentes Claude/Codex devem operar de forma restrita ao escopo da tarefa. Devem começar auditando Git quando solicitado, respeitar branch e worktree existentes, não reverter alterações do usuário e não executar operações destrutivas.

Regras obrigatórias para agentes:

- não usar sudo;
- não alterar código quando a tarefa for documental;
- não usar dados reais sem autorização;
- não rodar pipeline pesado fora do escopo;
- não depender de `output/` ou `log/` como fonte normativa;
- explicar exatamente o diff quando alterar;
- criar apenas artefatos permitidos;
- não fazer staging, commit, push, tag ou release sem autorização literal;
- reportar branch, HEAD, status, arquivos alterados, comandos, testes, riscos e próximo passo.

Quando uma tarefa futura exigir violar a premissa consolidada, o agente deve parar e declarar o bloqueio.

## 30. Regras para chunks de terminal

Quando um chunk depender do resultado do anterior, o agente deve informar um chunk, aguardar o resultado, avaliar e só então informar o próximo. Não deve empilhar comandos dependentes que impeçam análise intermediária.

Chunks independentes podem ser paralelizados quando isso não prejudicar clareza nem segurança. Comandos destrutivos ou irreversíveis são proibidos salvo autorização literal aplicável.

Para tarefas de hotfix documental, os chunks devem privilegiar leitura, auditoria Git, criação dos novos arquivos permitidos e validação leve. Não devem executar pipeline, dados reais ou testes pesados.

## 31. Atualizações consolidadas pós-H2R/H2R-C e linhagem de produtos

A linhagem materializada consolidada é:

`registros_importados_bruto.csv -> registros_importados.csv -> registros_importados_operacional_pre_painel.csv -> registros_corrig.csv -> registros_validados.csv`.

Sentido normativo de cada camada:

- `registros_importados_bruto.csv`: snapshot técnico/fiel de importação bruta;
- `registros_importados.csv`: camada canônica de importação, pré-transformação operacional pós-tokenização;
- `registros_importados_operacional_pre_painel.csv`: camada operacional pós-tokenização/pré-painel;
- `registros_corrig.csv`: camada operacional em correção/checkpoint auditável;
- `registros_validados.csv`: projeção final contratual SISMONITORA, quando ativada e apta.

Regras consolidadas:

- nenhum arquivo pode ter duas semânticas em momentos diferentes;
- se a camada muda, o nome muda;
- `registros_importados_operacional_pre_painel.csv` não substitui `registros_importados.csv`, `registros_corrig.csv` nem `registros_validados.csv`;
- é proibido sobrescrever `registros_importados.csv` com conteúdo pós-tokenização;
- é proibido reconstruir tardiamente `registros_importados.csv` a partir de `registros_corrig.csv`;
- auditoria não pode sobrescrever histórico por camada, produto ou contexto;
- `auditoria_registros_importados_resumo.csv` não pode apagar camada anterior;
- auditoria deve ser acumulativa ou segmentada por produto/camada/contexto;
- `registros_corrig.csv` permanece checkpoint operacional;
- `registros_validados.csv` permanece produto final contratual quando autorizado;
- H2R-C é transição compatível, não conclusão do motor único;
- produto novo sem catálogo, índice, auditoria final, README/manual/output e linhagem é violação.

## 32. Regra final de governança para dev futuro

Todo desenvolvimento futuro deve preservar o contrato-base, esta versão integral consolidada e a linhagem materializada. O projeto deve preferir intervenção mínima, auditável e reversível, com documentação suficiente para que a próxima etapa entenda a camada, o produto, o perfil e a regra aplicada.

Se uma tarefa futura exigir alterar o sentido de qualquer camada, o nome do produto, a auditoria, o catálogo, o README/manual/output, o índice e a documentação de linhagem devem ser atualizados de forma coerente. Nenhuma mudança de semântica pode ficar invisível.

A regra final é: contrato primeiro, linhagem explícita, produto com identidade própria, validação no estágio correto e parada obrigatória diante de violação de premissa consolidada.
