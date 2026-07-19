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
#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QLabel>
#include <QLineEdit>
#include <QSpinBox>
#include <QTabWidget>
#include <QVBoxLayout>
#include <QWidget>
#include <string>
#include <vector>

class QButtonGroup;
class QPushButton;
class QSlider;
class QStackedWidget;
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
 * an IR curve has exactly one short-rate process (Vasicek/CIR/Hull-White) -- the Process tab edits
 * scalar parameters directly rather than a component table -- plus a curve-shape preview chart FX
 * has no equivalent of, since FX publishes one scalar spot rather than a tenor grid.
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
    ProvenanceWidget* provenanceWidget() const override {
        return provenanceWidget_;
    }

private slots:
    void onSaveClicked();
    void onAddTemplateRow();
    void onRemoveTemplateRow();
    void onMoveTemplateRowUp();
    void onMoveTemplateRowDown();
    void onProcessFieldChanged();
    void onTemplateChanged();
    void onModeChanged();

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
    void refreshCharts();
    void syncTableFromModel();
    void rebuildModelFromTable();

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
    QTabWidget* tabWidget_;
    QWidget* provenanceTab_;
    ProvenanceWidget* provenanceWidget_;

    // Instrument tab.
    QComboBox* currencyCombo_;
    QComboBox* indexNameCombo_;
    QComboBox* fixedLegFrequencyCombo_;
    QCheckBox* enabledCheck_;
    QSpinBox* secondsSpin_;
    QLineEdit* sourceNameEdit_;

    // Process tab.
    QComboBox* engineCombo_;
    // Simple/Advanced mode toggle, mirroring FxSpotRateEditor's own -- Simple is slider+spin
    // pairs for quick exploration, Advanced is a compact table for precise direct entry. Both
    // surfaces edit the same four scalars; onModeChanged() syncs whichever becomes active.
    QButtonGroup* modeGroup_;
    QStackedWidget* modeStack_;
    QSlider* initialRateSlider_;
    QDoubleSpinBox* initialRateSpin_;
    QSlider* thetaSlider_;
    QDoubleSpinBox* thetaSpin_;
    QSlider* kappaSlider_;
    QDoubleSpinBox* kappaSpin_;
    QSlider* sigmaSlider_;
    QDoubleSpinBox* sigmaSpin_;
    QTableWidget* advancedTable_; // 1 row x 4 columns: r0, theta, kappa, sigma
    SampleShortRatePathsChart* pathsChart_;
    CurveShapePreviewChart* shapeChart_;

    // Curve Template tab.
    QTableWidget* templateTable_;
    QPushButton* addRowBtn_;
    QPushButton* removeRowBtn_;
    QPushButton* moveUpBtn_;
    QPushButton* moveDownBtn_;
    QLabel* templateWarningLabel_;

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
