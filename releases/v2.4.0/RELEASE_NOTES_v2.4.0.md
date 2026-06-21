# v2.4.0 - Modos de execução e operações atômicas no painel

Publicação pública `v2.4.0` do script do Alvo Global do Componente Campestre Savânico do Programa Monitora.

## Destaques

- Inclui `MONITORA_MODO_EXECUCAO` como chave central para alternar entre execução completa, execução sem PNG, estatísticas sem gráficos, parada controlada após `registros_corrig.csv` e fluxo `painel_e_parar`.
- Mantém como padrão público `MONITORA_MODO_EXECUCAO <- "completo"` e `MONITORA_OPCAO_ABRIR_PAINEL_CORRECOES <- "N"`.
- Adiciona operações semânticas atômicas no painel para exclusão de COLETAS, movimento de formas de vida, substituição de desconhecida, limpeza de outras formas de vida e movimento em lote de formas de vida por COLETAS.
- Reduz a fragmentação da fila do painel: ações do usuário são exibidas como operações semânticas, enquanto itens técnicos permanecem disponíveis para auditoria.
- Adiciona notificações de início e conclusão para operações potencialmente demoradas, com trava contra duplo clique e bloqueio de duplicidade semântica.
- Implementa movimento em lote de formas de vida para mover formas entre nativa, exótica e seca/morta, migrando automaticamente apenas ocorrências seguras e preservando casos ambíguos em relatório específico.
- Acrescenta auditorias de persistência pós-aplicação e pós-exportação para garantir que as correções persistam em `registros_corrig.csv`.
- Implementa sincronização final de `Encostam`/`tipo_forma_vida` como contrato derivado dos campos inferiores finais.
- Reforça a comparação pré/pós-correções com normalização defensiva de tipos em relatórios auxiliares, evitando falhas de `rbindlist()` por classes divergentes em campos contextuais.
- Mantém relatórios de ambiguidades com campos de localização: EA, UA, ciclo, campanha, ano, data, ponto amostral e ponto metro.

## Validação operacional

A versão foi validada com execução parcial `painel_e_parar`, incluindo:

- exclusões atômicas de COLETAS;
- movimento em lote de formas de vida;
- substituição de forma desconhecida;
- limpeza atômica de outras formas de vida;
- sincronização final de `Encostam`;
- auditorias pós-aplicação e pós-exportação sem falhas;
- exportação de `output/registros_corrig.csv` com persistência confirmada.

## Arquivos principais

- `monitora_campsav_alvo_global_v2.4.0.R`
- `monitora_campsav_alvo_global.R`
- `MONITORA_CAMPSAV_Alvo_Global.R`
- `R/monitora_campsav_alvo_global.R`
- `R_monitora_campsav_alvo_global.R`
- `README.md`
- `CHANGELOG.md`
- `VERSION`
- `SHA256SUMS.txt`
