# Audit du projet d'économétrie

**Rendu audité :** *Monetary Policy Forecasts and Shocks Spillovers of U.S. on Canada and Mexico: replication and extension of a structural VAR model* (Marcel Nguyen, Alexia Thomas Le Guennic, Harri Antoninis).
**Base :** réplication et extension de Stock & Watson, *Vector Autoregressions*, JEP 2001.
**Éléments examinés :** PDF du papier (27 p.) + dépôt `Project_Metrics_3` complet (code R `Prep_1→4`, `figure_1→3`, `extension_1`, `Extension_2`, données, figures).

---

## Avertissement de périmètre

L'énoncé du devoir (`1.pdf`) était **vide (0 octet)** : l'audit n'a pas pu être calé sur le barème exact. Il a donc été mené sur les critères standard d'un projet d'économétrie appliquée : fidélité de la réplication, rigueur des diagnostics, qualité de l'identification, reproductibilité du code, cohérence texte/résultats, présentation. Si l'énoncé réel est récupéré, certains points (pondération réplication vs extension, format imposé) mériteront une seconde passe.

---

## Verdict global

Travail solide et sérieux. La réplication est correcte sur le fond, le code s'exécute, le journal de bord (README) est honnête et détaillé, l'extension est ambitieuse (trois pays + sous-échantillons + règle de Taylor structurelle).

Le rendu perd surtout des points sur **la présentation** et sur **quelques incohérences vérifiables** — précisément ce qu'un correcteur cherche. La majorité des pertes sont **récupérables à faible coût** (insérer des figures, corriger un signe, relabelliser un tableau).

---

## Points forts (à conserver / valoriser)

- **Données conformes à Stock & Watson** : inflation = `400·log(GDPDEF_t / GDPDEF_{t-1})` (annualisée), moyennes trimestrielles des séries mensuelles, échantillon 1960Q1–2000Q4 (`Prep_1.R`).
- **Test de Granger correct** : test joint F sur les 4 retards via `linearHypothesis` ; l'orientation du tableau (cause vers effet) est cohérente avec le texte (`figure_1.R`).
- **Identification des spillovers bien posée** : ordre de Cholesky plaçant les variables américaines avant les variables domestiques (bloc-exogénéité des États-Unis), hypothèse légitime de petite économie ouverte (`extension_1.R`).
- **Appendice mathématique forward-looking** dérivé proprement et implémenté fidèlement : `F_k = (1/k)·Σ Φ^i`, point fixe `f_fwd = f_raw / (1 − fr_raw)` (`figure_3.R`, `compute_forward_coefficients`).
- **Hygiène des secrets** : clé API FRED lue depuis une variable d'environnement, jamais committée.
- **README / journal de bord** transparent sur les essais ratés (conventions de signe, instabilité du forward) : rassure sur la démarche.

---

## Problèmes classés par gravité

### BLOQUANTS (impact note direct, prioritaires)

**B1 — La section 3.1 (pré/post-2008) n'a aucun tableau ni figure dans le PDF final.**
Le texte (p. 8–10) affirme des résultats (« VAR(2) pré / VAR(4) post », « le KPSS s'améliore après 2008 », comparaison d'IRF) **sans aucun exhibit visible**. Le corps contient même des **références non résolues en clair** : « *tab_stationarity_pre and tab_stationarity_post* ». Or les figures existent déjà : `Extension/Extension_2/Figures/` (`irf_pre_post_comparison.jpeg`, `stationarity_precrisis.png`, `stationarity_postcrisis.png`, `lag_selection_precrisis.png`, `lag_selection_postcrisis.png`, `portmanteau_pre_post.png`, `roots_compare.png`, etc.).
→ Une extension entière est invisible alors que le travail est fait. **Plus grosse perte de points, réparation la plus simple : insérer les figures/tableaux et corriger les références.**

**B2 — Incohérence de signe dans la règle de Taylor backward (`figure_3.R`, l. 19-33).**
- Backward : `ra = r + fp_back*p − fu_back*u`
- Forward : `ra = r − fp_fwd*p − fu_fwd*u`

Le terme d'inflation entre avec **+ pour le backward** et **− pour le forward**, alors que l'appendice (p. 19) définit pour les deux : `ra = r − [fp/(1−…)]·p − [fu/(1−…)]·u`. De plus, `recover_irf` (l. 80) inverse avec la convention **forward** (`r_nominal = ra + fp*p + fu*u`) et l'applique **aussi au backward**. La Figure 3 (IRF backward vs forward, résultat phare) repose donc sur une transformation backward incohérente avec sa propre algèbre.
→ Réaligner : `ra_back = r − fp_back*p − fu_back*u`, puis régénérer la Figure 3 et vérifier si elle change.

**B3 — Tableau de stationnarité (Table 1, p. 14) incohérent et mal étiqueté.**
- Les statistiques PP (−12.77, −8.49, −11.34) sont franchement dans la zone de rejet, mais leurs p-values « ne rejettent pas » (0.38, 0.63, 0.47). Avec `tseries::pp.test`, une statistique de −12.77 donne mécaniquement p ≈ 0.01 : **le couple statistique/p-value est impossible tel quel**. À vérifier sur la sortie brute (probable confusion entre les statistiques Z(τ) et Z(ρ), ou colonnes décalées).
- La colonne intitulée « **DFGLS p-value** » contient en réalité **−1.94 = la valeur critique à 5 %** (le code stocke bien `DFGLS_cv5`, `Prep_2.R` l. 28).
→ Renommer la colonne « valeur critique 5 % » et corriger/vérifier la ligne PP. Sans cela, le tableau est faux pour le lecteur.

### IMPORTANTS

**I1 — `figure_3.R` n'est pas exécutable de haut en bas.** Il appelle `compute_forward_coefficients` (l. 171) **avant sa définition** (l. 184) et référence `var_precrisis`, `macro_monthly_precrisis` qui appartiennent à `Extension_2.R`. Réplication et extension pré/post-crise sont mélangées dans le même fichier.
→ Déplacer le bloc « k=4 / pré-post » vers `Extension_2.R`, ou documenter explicitement l'ordre d'exécution inter-fichiers.

**I2 — Comparaison Mexique restreint vs non-restreint sur des échantillons différents.** Non-restreint = `macro_mex_us_post2000` (en réalité filtré **≥ 1990**, `Prep_4.R` l. 152-155 — variable **mal nommée** « post2000 ») ; restreint = `macro_mex_us` (≥ 1987 complet). On ne peut pas comparer deux spécifications sur des fenêtres différentes.
De plus la conclusion **« le modèle complet surajuste »** (Canada) est **faiblement étayée** : la table MSE (Table 9) est mitigée — le complet l'emporte à h=1 et h=12, le restreint à h=4 et h=8.
→ Aligner les fenêtres ou expliciter la non-comparabilité ; nuancer la conclusion sur le surajustement.

**I3 — Erreur conceptuelle (p. 2).** « *reverse causality bias that yields an **unbiased** and unusable estimate* » : une causalité inverse donne un estimateur **biaisé**, pas « unbiased ». L'idée que le VAR « corrige » l'endogénéité est aussi imprécise : un VAR en forme réduite **contourne** le problème, l'interprétation structurelle dépendant ensuite de l'identification (Cholesky).
→ Reformuler.

**I4 — Horizons et niveaux de CI incohérents texte/code.** Le texte (§2.5–2.6) annonce des horizons « 1Q, 1 an, 2 ans, **4 ans** », mais le code prend `c(1,4,8,12)` = max **12 trimestres = 3 ans** (`figure_1.R` l. 89, 107). Les IRF de réplication utilisent **66 %** de CI, les extensions **95 %** (`extension_1.R`).
→ Corriger « 3 ans » dans le texte ; uniformiser ou justifier les niveaux de CI.

**I5 — Contradiction interne sur la règle forward.** L'appendice (p. 20) interprète économiquement les coefficients forward (« la FED pondère davantage le forward-looking »), alors que §3.1.3 et le README qualifient ces mêmes coefficients d'**instables numériquement** (dénominateur `1 − fr_raw ≈ 0` qui amplifie les coefficients) et d'**économiquement implausibles**.
→ Trancher : soit conditionner fortement l'interprétation, soit la retirer.

### MINEURS / COSMÉTIQUES (vite réglés, mais comptent sur un rendu noté)

- `Prep_4.R` l. 119-122 : `exr_mex` (« taux de change ») est calculé **à partir de la série de chômage** `unrate_mex` (copier-coller). Inoffensif car la colonne est ensuite supprimée, mais mauvais effet à la lecture.
- `extension_1.R` l. 375-384 : dans la section Mexique, `irf_can_r_rest` est recalculé sur le VAR **Canada** (copier-coller ; objet jamais utilisé).
- Table 12 (Mexique mensuel) : `FPE = 0` sur tous les retards (underflow d'affichage). À retirer ou afficher en notation scientifique.
- Typos / labels : « Quaterly » ; « **PCF** » au lieu de **PACF** (légende Fig. 5) ; « dependance / independance » ; « appropirate » ; « modelize ». P. 18 : le poids du chômage est noté deux fois `f⁰_p` (lire `f⁰_u = −1.25`). Coefficient forward : 0.757 (texte) vs 0.758 (code).
- Notation du VAR (p. 2) : `Σ_{p=1}^{T} A_{t-p} Y_{t-p}` est imprécise (coefficients constants `A_i`, somme jusqu'à l'ordre de retard `p`, pas `T`).
- `.DS_Store` committés (propreté du dépôt).

---

## Plan d'action priorisé (rapport effort / gain)

1. **Insérer les exhibits de la section 3.1** et résoudre les références cassées. *Gain élevé, effort faible.* (B1)
2. **Renommer la colonne « DFGLS p-value »** en « valeur critique 5 % » et vérifier la ligne PP. *Gain élevé, effort minime.* (B3)
3. **Corriger le signe de la règle backward** et régénérer la Figure 3. *Gain élevé, effort moyen.* (B2)
4. **Corriger « unbiased » → « biased »** (p. 2) et reformuler la motivation VAR/endogénéité. *Gain moyen, effort nul.* (I3)
5. **Harmoniser l'échantillon Mexique** (même fenêtre) ou expliciter la non-comparabilité ; **nuancer le « surajustement »** Canada. (I2)
6. **Trancher l'interprétation forward-looking** (la conditionner ou la retirer). (I5)
7. **Nettoyage** : horizons « 3 ans », CI uniformes, typos, copier-collés, renommer `post2000`. (I4 + mineurs)
