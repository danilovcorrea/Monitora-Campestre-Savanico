# Versionamento

As versões públicas identificam conjuntos reproduzíveis de software e produtos.
Cada publicação informa versão, build e hashes; rodadas anteriores são preservadas.

- Correções localizadas recebem incremento de patch.
- Ampliações compatíveis de recursos podem receber incremento menor.
- A v3.0.0 inaugura um ciclo consolidado de análises integradas, cartografia,
  relatórios e documentação, conforme decisão de versão do responsável pelo
  projeto. Esse marco não implica incompatibilidade dos arquivos de entrada.
- Iterações internas são consolidadas antes da publicação; não substituem
  silenciosamente o arquivo executado em uma rodada já concluída.

O identificador de build e o SHA-256 permitem distinguir revisões de uma mesma
candidata. A homologação registra os testes e as limitações efetivamente verificadas.
