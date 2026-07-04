# Plano executivo do motor único — status e trilha (2026-07-04)

Este documento não substitui o plano normativo, que está nas seções 27
("Plano incremental do motor único") e 28 ("Critérios de aceitação do
motor único") de
`diagnostics/contrato_governanca_dev_consolidado/CONTRATO_GOVERNANCA_DEV_MONITORA_CONSOLIDADO_INTEGRAL.md`.
Este documento é o **rastreamento operacional** de onde o script oficial
(`monitora_campsav_alvo_global_v2.6.0.R`) está, hoje, em relação a esse
plano — para que a próxima etapa não precise reauditar do zero.

## 1. O que já está implementado e em uso (verificado nesta etapa)

Etapas do plano normativo (seção 27) já cumpridas, com evidência de código:

1. **Contrato embutido** — `monitora_contrato_unico_embutido()` (linha
   aproximada 32120). Fonte única, sem dependência de XLSX/CSV/ZIP externo
   em runtime, conforme seção 4 do contrato.
2. **Perfis derivados / índices e caches** — `monitora_contrato_unico_indices()`
   (linha aproximada 32496). Recebe o contrato embutido e deriva
   `severidade`, `estágio aplicável` e demais anotações de orquestração
   **sem introduzir fatos novos** sobre o atributo (comentário normativo no
   próprio código confirma essa garantia).
3. **Diagnóstico opt-in conectado** — `monitora_contrato_unico_indices()` já
   é consumida por 6 pontos vivos do script, todos de natureza diagnóstica/
   comparativa, não de substituição do fluxo primário:
   - `monitora_contrato_unico_diagnosticar_observado_canonico()`
   - `monitora_registros_importados_diagnostico_contrato_unico()`
   - `monitora_registros_importados_comparar_ordem_legado_vs_contrato()`
   - `monitora_pipe_contrato_classificar_coluna()`
   - `monitora_pipe_contrato_resolver_tokens_isolado()`
   - `monitora_pipe_contrato_diagnosticar_dataset()`
4. **Camada de transparência sobre legado morto** —
   `monitora_motor_unico_importados_implementacoes_vivas_035n_e()` (linha
   aproximada 24584), helper somente leitura, não chamado pelo fluxo
   principal, que expõe qual definição de cada exportador de registros
   importados é viva vs. sobrescrita lexicalmente. Reduz ambiguidade sem
   alterar comportamento (ver
   `diagnostics/hotfix_035n_e_legado_morto_exportadores_importados/`).

## 2. O que ainda não está conectado (fontes paralelas continuam sendo o caminho primário)

Os seguintes itens da seção 27 **não têm evidência de conclusão** nesta
auditoria — os pontos de importação, pré-validação, painel, pós-validação,
exportação e estatísticas/gráficos continuam operando por lógica própria
(regras locais/hard-codes históricos), com o contrato único atuando apenas
como camada de diagnóstico/comparação em paralelo, não como fonte
determinante do comportamento:

- conectar importação (uso determinante, não apenas diagnóstico);
- conectar pré-validação;
- conectar painel;
- conectar pós-validação;
- conectar exportação;
- conectar estatísticas e gráficos;
- remoção de fontes paralelas (só pode ocorrer após equivalência
  comprovada — nenhuma removida ainda, corretamente, pois nenhuma conexão
  determinante foi feita ainda).

Esta auditoria não teve orçamento de tokens/tempo para inspecionar cada um
desses seis pontos em profundidade (o script tem ~48 mil linhas); a
lista acima é o **próximo alvo de auditoria**, não uma auditoria concluída
de ausência.

## 3. Decisão desta etapa: não introduzir novo código

Havia espaço, em tese, para dar mais um passo incremental (ex.: conectar
diagnóstico de pré-validação a `monitora_contrato_unico_indices()`).
**Decisão: não fazer isso nesta etapa.** Motivos:

- O contrato normativo (seção 2) exige evitar mudanças simultâneas em
  importação, painel, validação e exportação, e exige teste proporcional a
  cada alteração de risco de schema/cardinalidade/performance.
- Esta etapa já entrega três documentos de governança/arquitetura novos;
  somar uma alteração funcional no script de ~48 mil linhas sem uma rodada
  de teste dedicada (que esta etapa não orçou, por instrução explícita de
  economizar tokens e evitar pipeline pesado) elevaria o risco de regressão
  sem necessidade.
- Os dois incrementos mais recentes de código (`3d85944` e o helper
  035N-E) já foram entregues em commits anteriores, cada um com sua própria
  auditoria de impacto zero. Empilhar mais um incremento de código na mesma
  etapa que também mexe em documentação ampla não é proporcional.

## 4. Próximo incremento recomendado (não executado agora)

Auditar **um único ponto de conexão determinante** por vez, começando pelo
mais isolado e de menor blast radius: `monitora_registros_importados_diagnostico_contrato_unico()`
já compara o observado com o canônico — o próximo passo natural é avaliar
se esse diagnóstico pode virar bloqueio/alerta determinante (não apenas
relatório) para casos de alta severidade, com teste antes/depois sobre uma
massa de dados sintética (nunca dados reais) e comparação de contagem de
linhas/colunas do produto gerado. Critério de aceite: zero mudança de
schema ou cardinalidade em `registros_importados.csv` para o mesmo input,
antes e depois da conexão.

## 5. Critérios de aceitação (seção 28) — status resumido

| Critério | Status |
|---|---|
| Perfis derivados da mesma fonte | Parcial — índices existem, mas fluxo primário ainda não depende deles em todos os pontos |
| Importação sem fontes paralelas | Não — importação ainda não é determinantemente conectada |
| Pré-validação com diagnósticos suficientes | Parcial — diagnóstico de pipes e comparação legado-vs-contrato já existem |
| Painel exibindo regras/severidade/origem/bloqueio | Não auditado nesta etapa |
| Pós-validação determinando aptidão de `registros_corrig.csv` | Não auditado nesta etapa |
| Exportação com 129 colunas quando apto | Não auditado nesta etapa |
| Índices/caches deriváveis | Sim — `monitora_contrato_unico_indices()` |
| Produtos com identidade/linhagem próprias | Sim — ver arquitetura real materializada, seção 3 |
| Testes demonstrando equivalência | Parcial — auditorias documentais existem; sem suíte automatizada dedicada |
| Performance aceitável | Não medido nesta etapa (sem pipeline pesado, por decisão de economia) |
| Documentação refletindo a cadeia de produtos | Sim — README, contrato integral e este conjunto de documentos |

O motor único **ainda não** atende integralmente à seção 28; está em
progresso real e verificável, não apenas planejado.
