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
#ifndef ORES_QT_FX_SPOT_RATE_EDITOR_HPP
#define ORES_QT_FX_SPOT_RATE_EDITOR_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include "ores.synthetic.api/domain/gmm_component.hpp"
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
class ReturnDistributionChart;
class SamplePricePathsChart;

/**
 * @brief Tabbed detail editor for an FX spot rate simulation.
 *
 * Replaces the former FxPairDialog + ComponentDialog modal flow with a single
 * standard DetailDialogBase editor shown in an MDI sub-window. It edits the
 * fx_spot_generation_config together with its gmm_component price-model stack
 * in one place (Instrument, Update frequency, Price behaviour, Provenance).
 */
class FxSpotRateEditor final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.fx_spot_rate_editor";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct an editor for a new FX spot rate under @p parentFeedId.
     */
    FxSpotRateEditor(ClientManager* cm,
                     ImageCache* imageCache,
                     ChangeReasonCache* crCache,
                     const QString& username,
                     const boost::uuids::uuid& parentFeedId,
                     const QString& feedName,
                     QWidget* parent = nullptr);

    /**
     * @brief Construct an editor editing an existing FX spot rate.
     */
    FxSpotRateEditor(ClientManager* cm,
                     ImageCache* imageCache,
                     ChangeReasonCache* crCache,
                     const QString& username,
                     const synthetic::domain::fx_spot_generation_config& existing,
                     const QString& feedName,
                     const std::vector<synthetic::domain::gmm_component>& components,
                     QWidget* parent = nullptr);

    ~FxSpotRateEditor() override = default;

    QSize sizeHint() const override {
        return QSize(1140, 740);
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
    void onCurrencyChanged();
    void onSaveClicked();
    void onAddComponentRow();
    void onModeChanged();
    void onResetSimple();
    void onResetAdvanced();

private:
    // The single source of truth for the price model: one GMM component.
    struct ModelComponent {
        std::string id; // existing component id, or empty for new
        std::string description;
        double mean = 0.0;
        double stdev = 0.0;
        double weight = 0.0;
    };

    void buildUi();
    void buildInstrumentTab();
    void buildBehaviourTab();
    // left pane (Simple): a QStackedWidget switching between the GBM/arithmetic
    // sliders and "ou"'s dedicated θ/κ/σ controls, per the active engine.
    QWidget* buildSimpleControls();
    QWidget* buildGbmSimpleControls();  // Simple page for mixing engines
    QWidget* buildOuSimpleControls();   // Simple page for "ou"
    QWidget* buildAdvancedControls(); // left pane (Advanced): table + add/reset
    void populateCurrencyCombo(QComboBox* combo);
    void recomputeOreKey();
    void recomputeDefaultSourceName();
    void recomputeFrequencyEcho();

    // Single-source-of-truth syncing between the two editing surfaces + charts.
    void syncSimpleFromModel();      // model -> sliders + simple type combo
    void syncAdvancedFromModel();    // model -> table
    void rebuildModelFromSimple();   // sliders -> model
    void rebuildModelFromAdvanced(); // table -> model
    void refreshCharts();            // model -> both charts + weight-sum label
    void updateWeightSumLabel();     // weight-sum label text (or κ echo for "ou")
    // "ou"'s Simple page: θ/κ/σ controls, dispatched from the functions above.
    void syncOuSimpleFromModel();
    void rebuildOuModelFromSimple();
    void onResetOuSimple();

    // Advanced table row construction; returns nothing, appends to table.
    void addTableRow(const ModelComponent& c);
    void applyProfileToRow(int row, const QString& profile); // fills σ
    void updateRemoveButtonsEnabled(); // disable Remove when only one row remains
    // Re-paint each row's colour swatch to match its current row index (colour
    // indices shift when a row is removed from the middle of the table).
    void updateComponentColors();

    void onEngineChanged();
    void updateEngineUi(); // relabel headers/tooltips/warning and gate Add for "ou"
    [[nodiscard]] std::string currentEngine() const; // "geometric" / "arithmetic" / "ou"
    [[nodiscard]] bool currentEngineSupportsMixing() const;
    [[nodiscard]] QString incrementNoun() const; // label noun for the active engine
    // Detailed hover text for a component row's Name/μ/σ/Weight cells, explaining
    // what those fields mean for the current engine.
    [[nodiscard]] QString componentTooltip(bool ou, bool arithmetic) const;

    // Ornstein-Uhlenbeck's κ is a per-tick reversion rate — realistic values span
    // many orders of magnitude (minutes to months), which is unusable to type
    // directly (e.g. 0.000001). The Weight cell edits/displays half-life in
    // minutes instead; these convert to/from the κ actually stored and sent to
    // the simulation engine. Half-life is expressed relative to the *current*
    // "New price every" tick interval — changing that interval afterwards does
    // not retroactively rescale an already-entered κ (same as the interval's
    // effect on any other per-tick parameter).
    [[nodiscard]] double secondsPerTick() const;
    [[nodiscard]] double halfLifeMinutesFromKappa(double kappa) const;
    [[nodiscard]] double kappaFromHalfLifeMinutes(double halfLifeMinutes) const;

    [[nodiscard]] QString defaultSourceName() const;
    [[nodiscard]] std::vector<ModelComponent> currentComponents() const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString username_;
    QString feedName_;
    bool isNew_;
    bool userEditedSource_{false};
    bool syncing_{false}; // guard against feedback loops while syncing surfaces

    synthetic::domain::fx_spot_generation_config fx_;
    // Ids of components that existed when editing began (to compute deletions).
    std::vector<std::string> originalComponentIds_;

    // The price-model source of truth.
    std::vector<ModelComponent> components_;

    // Tabs.
    QTabWidget* tabWidget_;
    QWidget* provenanceTab_;
    ProvenanceWidget* provenanceWidget_;

    // Instrument tab.
    QComboBox* baseCombo_;
    QComboBox* quoteCombo_;
    QLabel* oreKeyLabel_;
    QLineEdit* sourceNameEdit_;
    QDoubleSpinBox* priceSpin_;
    QCheckBox* enabledCheck_;

    // Frequency tab.
    QSpinBox* secondsSpin_;
    QLabel* frequencyEchoLabel_;

    // Behaviour tab — shared.
    QComboBox* engineCombo_;
    QLabel* engineWarningLabel_;
    QButtonGroup* modeGroup_;
    QStackedWidget* modeStack_;
    // Single shared charts (used by both Simple and Advanced modes).
    ReturnDistributionChart* distChart_; // compact, top-right
    // Shown instead of distChart_ for single-regime engines (the PDF doesn't
    // apply), so the panel doesn't burn space on a disabled chart.
    QLabel* distInfoLabel_;
    SamplePricePathsChart* pathsChart_; // prominent, full-width bottom

    // Behaviour tab — Simple page. simpleModeStack_ switches between the GBM/
    // arithmetic sliders (index 0) and "ou"'s θ/κ/σ controls (index 1).
    QStackedWidget* simpleModeStack_;
    QSlider* driftSlider_;
    QSlider* volSlider_;
    QSlider* jumpSlider_;
    QLabel* driftValueLabel_;
    QLabel* volValueLabel_;
    QLabel* jumpValueLabel_;

    // Behaviour tab — Simple page, "ou" controls.
    QLabel* ouThetaLabel_;   // read-only echo of priceSpin_ (θ is edited there)
    QSlider* kappaSlider_;   // log-mapped, see kappaSliderToValue/kappaValueToSlider
    QDoubleSpinBox* kappaSpin_;
    QLabel* ouHalfLifeLabel_;
    QSlider* ouSigmaSlider_;
    QDoubleSpinBox* ouSigmaSpin_;

    // Behaviour tab — Advanced page.
    QTableWidget* componentTable_;
    QPushButton* addComponentBtn_; // disabled for the single-regime "ou" engine
    QLabel* weightSumLabel_;

    std::vector<std::string> knownCodes_;
};

}

#endif
