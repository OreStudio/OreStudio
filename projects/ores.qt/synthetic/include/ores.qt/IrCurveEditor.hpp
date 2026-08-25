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
#ifndef ORES_QT_IR_CURVE_EDITOR_HPP
#define ORES_QT_IR_CURVE_EDITOR_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config.hpp"
#include "ores.synthetic.api/domain/ir_curve_generation_config_process_parameter_value.hpp"
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/domain/yield_curve_process_parameter_definition.hpp"
#include "ores.synthetic.api/messaging/simulate_ir_curve_paths_protocol.hpp"
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QLineEdit>
#include <QPointer>
#include <QSpinBox>
#include <QTabWidget>
#include <QVBoxLayout>
#include <QWidget>
#include <string>
#include <vector>

class QDoubleSpinBox;
class QPushButton;
class QRadioButton;
class QTableWidget;

namespace ores::qt {

class ImageCache;
class ChangeReasonCache;
class ProvenanceWidget;
class SampleShortRatePathsChart;
class CurveShapePreviewChart;

/**
 * @brief Tabbed detail editor for an IR curve generation process, the rates analogue of
 * FxSpotRateEditor.
 *
 * Edits the ir_curve_generation_config together with its ir_curve_template_entry tenor grid in
 * one place (Instrument, Process, Curve Template, Provenance). Unlike FX's gmm_component mixture,
 * an IR curve has exactly one short-rate process (Vasicek/Cox-Ingersoll-Ross/Hull-White/
 * Two-Factor Gaussian) -- the Process tab edits its parameters row-based, one table row per
 * yield_curve_process_parameter_definition (name, description, min/max, default) with the
 * config's own value row -- plus a curve-shape preview chart FX has no equivalent of, since FX
 * publishes one scalar spot rather than a tenor grid. Adding a process type (or parameter) is
 * seed data only: the table is rebuilt from the definitions catalogue whenever the engine
 * changes.
 */
class IrCurveEditor final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.ir_curve_editor";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /** @brief Construct an editor for a new IR curve process under @p parentFeedId. */
    IrCurveEditor(ClientManager* cm,
                  ImageCache* imageCache,
                  ChangeReasonCache* crCache,
                  const QString& username,
                  const boost::uuids::uuid& parentFeedId,
                  const QString& feedName,
                  QWidget* parent = nullptr);

    /** @brief Construct an editor editing an existing IR curve process. */
    IrCurveEditor(ClientManager* cm,
                  ImageCache* imageCache,
                  ChangeReasonCache* crCache,
                  const QString& username,
                  const synthetic::domain::ir_curve_generation_config& existing,
                  const QString& feedName,
                  const std::vector<synthetic::domain::ir_curve_template_entry>& entries,
                  QWidget* parent = nullptr);

    ~IrCurveEditor() override = default;

    QSize sizeHint() const override {
        return QSize(1100, 760);
    }

signals:
    void savedOk();
    void statusChanged(const QString& message);
    void errorOccurred(const QString& message);

protected:
    QTabWidget* tabWidget() const override {
        return tabWidget_;
    }
    QWidget* provenanceTab() const override {
        return provenanceTab_;
    }
    ProvenanceWidget* provenanceWidget() const override;

private slots:
    void onSaveClicked();
    void onAddTemplateRow();
    void onRemoveTemplateRow();
    void onMoveTemplateRowUp();
    void onMoveTemplateRowDown();
    void onProcessFieldChanged();
    void onTemplateChanged();
    void onBrowseVintageClicked();

private:
    // One Curve Template row, the single source of truth for the table (mirrors
    // FxSpotRateEditor::ModelComponent's role for gmm_component rows).
    struct TemplateRow {
        std::string id; // existing entry id, or empty for new
        std::string start_tenor_code;
        std::string end_tenor_code;
        std::string instrument_code;
    };

    void buildUi();
    void buildInstrumentTab();
    void buildProcessTab();
    void buildCurveTemplateTab();

    void populateCurrencyCombo();
    void populateIndexNameCombo();
    void populatePaymentFrequencyCombo();
    void populateTenorCodes();
    void populateInstrumentCodes();

    // Mirrors FxSpotRateEditor's defaultSourceName()/recomputeDefaultSourceName(): same
    // "synthetic.<collection>.<pair>" shape, same "don't overwrite once the user has typed their
    // own" rule (userEditedSource_).
    [[nodiscard]] QString defaultSourceName() const;
    void recomputeDefaultSourceName();
    // Mirrors FxSpotRateEditor::recomputeOreKey(): a selectable label showing the exact
    // feed_binding.ore_key SyntheticBindingDialog will compute for this config, so the tester
    // doesn't have to work it out by hand when binding.
    void recomputeOreKey();
    void refreshCharts();
    void syncTableFromModel();
    void rebuildModelFromTable();

    // Process tab: fetch the selected engine's parameter definitions + this config's value rows
    // (edit mode), then rebuild parameterTable_. Async, mirroring populateTenorCodes()'s watcher
    // pattern -- the editor is self-contained and the caller doesn't pass either in.
    void populateParameterRows();
    void rebuildParameterTable();
    void updatePriceSourceEnablement();
    [[nodiscard]] QDoubleSpinBox* valueSpinAt(int row) const;
    // Current parameter values from the table, in definition display_order -- the shape the two
    // preview charts' setParameters() and the mapping layer consume.
    [[nodiscard]] std::vector<ores::synthetic::messaging::parameter_spec> currentParameters() const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString username_;
    QString feedName_;
    bool isNew_;
    bool userEditedSource_{false}; // mirrors FxSpotRateEditor's own flag

    synthetic::domain::ir_curve_generation_config ir_;
    std::vector<std::string> originalEntryIds_;
    std::vector<TemplateRow> entries_;

    // Tabs.
    QPointer<QTabWidget> tabWidget_;
    QPointer<QWidget> provenanceTab_;
    QPointer<ProvenanceWidget> provenanceWidget_;

    // Instrument tab.
    QPointer<QComboBox> currencyCombo_;
    QPointer<QComboBox> indexNameCombo_;
    QPointer<QComboBox> roleCombo_;
    QPointer<QLabel> oreKeyLabel_;
    QPointer<QComboBox> fixedLegFrequencyCombo_;
    QPointer<QCheckBox> enabledCheck_;
    QPointer<QSpinBox> secondsSpin_;
    QPointer<QLineEdit> sourceNameEdit_;
    // Price source: mirrors FxSpotRateEditor's own radio-group pattern (see its header's
    // priceSourceGroup_ comment). Checking "Vintage" makes vintageSourceEdit_/vintageDateEdit_
    // authoritative and disables the Process tab's initial-rate parameter row (initialRateSpin_),
    // since the starting rate is then resolved server-side from a real DEPOSIT-tenor observation
    // -- see ir_curve_generation_config.price_source.
    QPointer<QRadioButton> fixedRadio_;
    QPointer<QRadioButton> vintageRadio_;
    QPointer<QButtonGroup> priceSourceGroup_;
    QPointer<QLineEdit> vintageSourceEdit_;
    QPointer<QLineEdit> vintageDateEdit_;
    QPointer<QPushButton> browseVintageButton_;

    // Process tab.
    QPointer<QComboBox> engineCombo_;
    // One table row per yield_curve_process_parameter_definition for the selected engine:
    // read-only parameter name/description (column 0) + a QDoubleSpinBox per value row (column
    // 1), the spin's range clamped to the definition's min/max and its value seeded from the
    // config's value row (edit mode) or the definition's default_value. Rebuilt by
    // populateParameterRows() whenever the engine changes -- the single editing surface, precise
    // entry included, so there is no Simple/Advanced split to keep in sync with a dynamic
    // parameter set (4 rows for the one-factor engines, 7 for Two-Factor Gaussian).
    QPointer<QTableWidget> parameterTable_;
    // The initial_rate spin box, looked up when the table is rebuilt -- the one parameter the
    // vintage price source overrides server-side, so its editor control is disabled in vintage
    // mode (see updatePriceSourceEnablement()).
    QPointer<QDoubleSpinBox> initialRateSpin_;
    QPointer<QLabel> parametersLoadingLabel_;
    std::vector<synthetic::domain::yield_curve_process_parameter_definition>
        parameterDefinitions_; // current engine's, sorted by display_order
    std::vector<synthetic::domain::ir_curve_generation_config_process_parameter_value>
        valueRows_; // the config's existing value rows (edit mode): seeds the table's spins, and
                    // save compares against them to find stale rows for deletion
    QPointer<SampleShortRatePathsChart> pathsChart_;
    QPointer<CurveShapePreviewChart> shapeChart_;

    // Curve Template tab.
    QPointer<QTableWidget> templateTable_;
    QPointer<QPushButton> addRowBtn_;
    QPointer<QPushButton> removeRowBtn_;
    QPointer<QPushButton> moveUpBtn_;
    QPointer<QPushButton> moveDownBtn_;
    QPointer<QLabel> templateWarningLabel_;

    std::vector<std::string> knownCurrencyCodes_;
    // Full floating_index_type codes (e.g. "USD-SOFR") -- overnight-style only (two "-"-delimited
    // segments), excluding term-IBOR tenor variants (e.g. "EUR-EURIBOR-6M") which make no sense as
    // a single curve's identity. See indexNameCombo_'s own population for the heuristic.
    std::vector<std::string> knownFloatingIndexCodes_;
    std::vector<std::string> knownTenorCodes_;
    std::vector<std::string> knownInstrumentCodes_;
    std::vector<std::string> knownPaymentFrequencyCodes_;
    bool syncing_{false};
};

}

#endif
