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

namespace ores::qt {

class ImageCache;
class ChangeReasonCache;
class ProvenanceWidget;

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
        return QSize(640, 620);
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
    void onAddProcess();

private:
    // Process type, inferred from / mapped to gmm_component.mean.
    enum class ProcessType { DriftlessGbm = 0, GbmWithDrift = 1 };

    // One editable process card in the stack.
    struct ProcessCard {
        QWidget* container;
        QComboBox* typeCombo;
        QComboBox* profileCombo;
        QWidget* driftRow;      // holds the μ label + spin; hidden for driftless
        QDoubleSpinBox* meanSpin;
        QDoubleSpinBox* stdevSpin;
        QDoubleSpinBox* weightSpin;
        QLineEdit* descEdit;
        std::string id; // existing component id, or empty for a new card
    };

    void buildUi();
    void buildInstrumentTab();
    void buildFrequencyTab();
    void buildBehaviourTab();
    void populateCurrencyCombo(QComboBox* combo);
    void recomputeOreKey();
    void recomputeDefaultSourceName();
    void recomputeFrequencyEcho();
    void recomputeWeightSum();

    void addProcessCard(ProcessType type, const QString& profile, double mean, double stdev,
                        double weight, const QString& desc, const std::string& id = {});
    void clearProcessCards();
    void renumberCards();
    void applyProfileToCard(ProcessCard& card, const QString& profile);
    void applyTypeToCard(ProcessCard& card);

    [[nodiscard]] QString defaultSourceName() const;

    ClientManager* clientManager_;
    ImageCache* imageCache_;
    QString username_;
    QString feedName_;
    bool isNew_;
    bool userEditedSource_{false};

    synthetic::domain::fx_spot_generation_config fx_;
    // Ids of components that existed when editing began (to compute deletions).
    std::vector<std::string> originalComponentIds_;

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

    // Behaviour tab.
    QVBoxLayout* stackLayout_;
    QLabel* weightSumLabel_;
    std::vector<ProcessCard> cards_;

    std::vector<std::string> knownCodes_;
};

}

#endif
