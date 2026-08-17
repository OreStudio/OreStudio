/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_QT_CURVE_BUILDER_WORKBENCH_HPP
#define ORES_QT_CURVE_BUILDER_WORKBENCH_HPP

#include "ores.marketdata.api/messaging/curve_republish_protocol.hpp"
#include "ores.qt/RefdataExport.hpp"
#include "ores.refdata.api/domain/floating_index_type.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_config.hpp"
#include "ores.refdata.api/domain/ir_curve_bootstrap_pillar.hpp"
#include <QWidget>
#include <functional>
#include <optional>
#include <unordered_map>
#include <vector>

class QComboBox;
class QDateTimeEdit;
class QLabel;
class QLineEdit;
class QListWidget;
class QPushButton;
class QTableWidget;
class QChart;
class QChartView;

namespace ores::qt {

class ClientManager;
class ImageCache;
class ChangeReasonCache;

/**
 * @brief Non-modal workbench replacing the standard modal detail dialog for
 * ir_curve_bootstrap_config: define a recipe, see its bootstrapped result, publish it -- an
 * iterative flow, not a save-and-close form, so it stays open across Save/Bootstrap/Publish
 * rather than closing after each action.
 *
 * Three tabs -- Conventions (recipe fields), Pillars (an editable grid, not a separate list
 * window per ir_curve_template_entry's own precedent -- deliberately diverging, see the task's
 * own Plan for why), Build & Diagnostics (as-of, results, a discount-factor plot and a
 * forward-rate health-check plot, both via the same QtCharts dark-theme pattern
 * CurveSnapshotMdiWindow already uses).
 *
 * Three distinct, explicit actions -- Save (persists the recipe only), Bootstrap (computes and
 * displays a preview, publishes nothing -- calls curve_republish_service::compute() via
 * compute_curve_request), Publish (makes the most recent Bootstrap's generation live -- calls
 * curve_republish_service::republish() via the existing republish_curve_request) -- never
 * fused. Bootstrap is disabled until the recipe has been saved at least once and has >= 1
 * pillar; Publish is disabled until a Bootstrap has run against the currently-saved recipe.
 */
class ORES_QT_REFDATA_EXPORT CurveBuilderWorkbench : public QWidget {
    Q_OBJECT

public:
    explicit CurveBuilderWorkbench(QWidget* parent = nullptr);

    /// Also kicks off the Currency/Index combo data load (floating_index_type list), since that
    /// needs a live ClientManager and this is the first setter the controller is guaranteed to
    /// call regardless of construction-site variant.
    void setClientManager(ClientManager* clientManager);
    /// Applies flag icons to the Currency combo once available -- independent of setClientManager
    /// call order, since the two setters are unordered from the controller's side.
    void setImageCache(ImageCache* imageCache);
    void setChangeReasonCache(ChangeReasonCache* changeReasonCache) {
        changeReasonCache_ = changeReasonCache;
    }
    void setUsername(const std::string& username) {
        username_ = username;
    }

    /// New-recipe mode: no existing config -- shown blank, user picks Source/Output Series Id via
    /// MarketSeriesPickerDialog (which can also create the output series inline, since
    /// curve_republish_service::compute() requires it to already be catalogued before the first
    /// Publish) and Saves.
    void setCreateMode(bool createMode);

    /// Loads an existing config + its pillars into the workbench.
    void setConfig(const refdata::domain::ir_curve_bootstrap_config& config);

    /// The config's own id as a string, or empty in create mode before the first Save --
    /// mirrors DetailDialogBase::code() for the controller's window-tracking/notification code,
    /// without inheriting that base (its save-and-close assumptions don't fit this workbench).
    [[nodiscard]] QString code() const;

    /// Mirrors DetailDialogBase::markAsStale() for the controller's notifyOpenDialogs() -- warns
    /// that another client has changed this same config since it was loaded here, without
    /// inheriting that base (see code()'s own comment for why).
    void markAsStale();

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& message);
    void closeRequested();
    /// Emitted after a successful Save, mirroring {Entity}DetailDialog::configSaved -- lets the
    /// controller call handleEntitySaved() (list refresh) the same way it does for the
    /// generated dialog, without this class depending on DetailDialogBase.
    void configSaved(const QString& id);

private slots:
    void onSaveClicked();
    void onBootstrapClicked();
    void onPublishClicked();
    void onAddPillarClicked();
    void onRemovePillarClicked();
    /// Create-mode helper: opens a picker over every existing config (any curve_family_role),
    /// clones its Conventions + Pillars into this blank recipe (fresh pillar ids, since a pillar
    /// id is unique per row regardless of parent config), and leaves Source/Output Series/
    /// split tenor for the user to fill in themselves, since those are always tenant/series-
    /// specific. A stopgap for novice-unfriendly blank-slate curve building until a proper
    /// curated template entity exists (see the story's own follow-on task).
    void onNewFromExistingClicked();

private:
    void buildUi();
    QWidget* buildConventionsTab();
    QWidget* buildPillarsTab();
    QWidget* buildDiagnosticsTab();

    void loadConfigIntoUi();
    void loadPillarsIntoTable();
    void collectConfigFromUi();
    void collectPillarsFromTable();
    void updateActionStates();
    void showBanner(const QString& message, bool isError);
    /// Error path for anything returned by the server (Save/Bootstrap/Publish failures): sets the
    /// banner to @p shortMessage for in-context visibility, and additionally pops a standard
    /// MessageBoxHelper::critical with @p details (the full technical error text, e.g. a
    /// repository/SQL error) in its expandable "Details" section -- a banner alone buried the raw
    /// error text inline with no way to copy it out or be sure the user actually saw it.
    void showError(const QString& title, const QString& shortMessage, const QString& details);
    void
    renderBootstrapResults(const std::vector<marketdata::messaging::computed_curve_point>& points);
    void onBrowseSourceSeriesClicked();
    void onBrowseOutputSeriesClicked();
    void onBrowseDiscountCurveConfigClicked();
    void loadFloatingIndexTypes();
    void refreshCurrencyCombo();
    void refreshIndexCombo();
    void loadTenors();
    /// Builds a non-editable combo populated from tenorCodes_ (Reference Data's seeded tenor
    /// table), so a pillar's Start/End Tenor can never hold an invalid code the way free text
    /// let it (e.g. "0D", which curve_republish_resolver rejects -- the real seeded spot-starting
    /// code is "SPOT"). @p initial selects that entry if present, else leaves no selection.
    QComboBox* makeTenorCombo(const QString& initial);
    /// bootstrap_curve_role_code's three fixed values (DEPOSIT/FRA/SWAP) -- same rationale as
    /// makeTenorCombo(), a small closed vocabulary that free text let drift.
    QComboBox* makeCurveRoleCombo(const QString& initial);
    /// The currently selected index code (e.g. "USD-SOFR"), or empty until both Currency and
    /// Index are chosen -- used to pre-filter the series pickers and the discount-curve picker.
    [[nodiscard]] QString selectedIndexCode() const;
    /// selectedIndexCode() translated from floating_index_type's dash convention (e.g.
    /// "USD-SOFR") to a slash convention (e.g. "USD/SOFR") -- what the series pickers' URI
    /// builders (fixing_uri_of/discount_curve_uri_of) actually need to derive the oresmd URI
    /// from.
    [[nodiscard]] QString selectedIndexQualifier() const;
    [[nodiscard]] QString selectedCurrency() const;
    /// Prompts via ChangeReasonDialog (same class DetailDialogBase::promptChangeReason() wraps,
    /// replicated standalone here since this workbench deliberately doesn't inherit that base --
    /// see its own class docs). Returns false (and shows an error banner) if the cache isn't
    /// ready, no reasons are configured, or the user cancels; on success stashes the pair into
    /// pendingChangeReasonCode_/pendingChangeCommentary_ for collectConfigFromUi()/
    /// collectPillarsFromTable() to apply.
    bool promptAndStashChangeReason();
    /// Fetches every pillar belonging to @p configId (client-side filtered -- the list request
    /// has no server-side config-id filter) and hands the sorted-by-sequence_index result to
    /// @p onLoaded on the UI thread. Shared by setConfig() (load this config's own pillars) and
    /// onNewFromExistingClicked() (clone another config's pillars).
    void fetchPillarsForConfig(
        const boost::uuids::uuid& configId,
        std::function<void(std::vector<refdata::domain::ir_curve_bootstrap_pillar>)> onLoaded);

    ClientManager* clientManager_ = nullptr;
    ImageCache* imageCache_ = nullptr;
    ChangeReasonCache* changeReasonCache_ = nullptr;
    std::string username_;
    bool createMode_ = true;
    refdata::domain::ir_curve_bootstrap_config config_;
    std::vector<refdata::domain::ir_curve_bootstrap_pillar> pillars_;
    bool hasBootstrapped_ = false;
    std::vector<refdata::domain::floating_index_type> floatingIndexTypes_;
    std::vector<std::string> tenorCodes_;
    /// tenor.sort_order by code -- the tenor ladder's own chronological position, used to derive
    /// each pillar's sequence_index from its End Tenor rather than from the Pillars table's row
    /// order (see collectPillarsFromTable()'s own comment for why row order alone is unsafe).
    std::unordered_map<std::string, int> tenorSortOrderByCode_;
    std::string pendingChangeReasonCode_;
    std::string pendingChangeCommentary_;

    // Conventions tab. Currency/Index gate everything below them: they constrain which market
    // series the Source/Output Series pickers offer, so a novice can't accidentally bootstrap a
    // USD curve off an FX or JPY series. sourceSeriesIdEdit_/outputSeriesIdEdit_/
    // discountCurveConfigIdEdit_ are read-only display fields fed by a picker dialog (Browse...
    // button) rather than free-typed UUIDs -- the ids themselves live only in config_, never
    // re-parsed from displayed text.
    QPushButton* newFromExistingButton_ = nullptr;
    QComboBox* currencyCombo_ = nullptr;
    QComboBox* indexCombo_ = nullptr;
    QLineEdit* sourceSeriesIdEdit_ = nullptr;
    QPushButton* browseSourceSeriesButton_ = nullptr;
    QLineEdit* outputSeriesIdEdit_ = nullptr;
    QPushButton* browseOutputSeriesButton_ = nullptr;
    QComboBox* curveFamilyRoleCombo_ = nullptr;
    QLineEdit* discountCurveConfigIdEdit_ = nullptr;
    QPushButton* browseDiscountCurveConfigButton_ = nullptr;
    QComboBox* interpolationMethodCombo_ = nullptr;
    QComboBox* dayCountConventionCombo_ = nullptr;
    QLineEdit* splitTenorCodeEdit_ = nullptr;
    QLabel* conventionsErrorLabel_ = nullptr;

    // Pillars tab
    QTableWidget* pillarsTable_ = nullptr;
    QPushButton* addPillarButton_ = nullptr;

    // Build & Diagnostics tab
    QDateTimeEdit* asOfEdit_ = nullptr;
    QLabel* bannerLabel_ = nullptr;
    QPushButton* saveButton_ = nullptr;
    QPushButton* bootstrapButton_ = nullptr;
    QPushButton* publishButton_ = nullptr;
    QLabel* bootstrapHintLabel_ = nullptr;
    QLabel* publishHintLabel_ = nullptr;
    QTableWidget* resultsTable_ = nullptr;
    QChart* dfChart_ = nullptr;
    QChartView* dfChartView_ = nullptr;
    QChart* forwardChart_ = nullptr;
    QChartView* forwardChartView_ = nullptr;
    QListWidget* healthFindingsList_ = nullptr;
};

}

#endif
