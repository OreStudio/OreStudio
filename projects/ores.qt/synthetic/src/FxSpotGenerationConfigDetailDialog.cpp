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
#include "ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ui_FxSpotGenerationConfigDetailDialog.h"
#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QSpinBox>
#include <QtConcurrent>
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxSpotGenerationConfigDetailDialog::FxSpotGenerationConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::FxSpotGenerationConfigDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

FxSpotGenerationConfigDetailDialog::~FxSpotGenerationConfigDetailDialog() {
    delete ui_;
}

QTabWidget* FxSpotGenerationConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FxSpotGenerationConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FxSpotGenerationConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FxSpotGenerationConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FxSpotGenerationConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &FxSpotGenerationConfigDetailDialog::onSaveClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &FxSpotGenerationConfigDetailDialog::onCloseClicked);

    connect(ui_->configCombo,
            &QComboBox::currentIndexChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->sourceNameEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->oreKeyEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->initialPriceSpin,
            &QDoubleSpinBox::valueChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->ticksPerHourSpin,
            &QSpinBox::valueChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->enabledCheck,
            &QCheckBox::toggled,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
}

void FxSpotGenerationConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateConfigCombo();
}

void FxSpotGenerationConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FxSpotGenerationConfigDetailDialog::setConfig(
    const synthetic::domain::fx_spot_generation_config& config) {
    config_ = config;
    updateUiFromConfig();
}

void FxSpotGenerationConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::populateConfigCombo() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    if (findChild<QFutureWatcherBase*>("configComboWatcher"))
        return;

    BOOST_LOG_SEV(lg(), debug) << "Populating generation config combo";

    QPointer<FxSpotGenerationConfigDetailDialog> self = this;

    struct FetchResult {
        bool success;
        std::vector<synthetic::domain::market_data_generation_config> configs;
    };

    QFuture<FetchResult> future = QtConcurrent::run([self]() -> FetchResult {
        if (!self || !self->clientManager_) {
            return {false, {}};
        }

        synthetic::messaging::get_market_data_generation_configs_request request;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));
        if (!response_result) {
            return {false, {}};
        }

        return {true, std::move(response_result->configs)};
    });

    auto* watcher = new QFutureWatcher<FetchResult>(self);
    watcher->setObjectName("configComboWatcher");
    connect(watcher, &QFutureWatcher<FetchResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self)
            return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error) << "Failed to fetch generation configs for combo box.";
            emit self->errorMessage(tr("Could not load generation configs."));
            return;
        }

        self->ui_->configCombo->blockSignals(true);
        self->ui_->configCombo->clear();

        for (const auto& config : result.configs) {
            const QString id = QString::fromStdString(boost::uuids::to_string(config.id));
            self->ui_->configCombo->addItem(QString::fromStdString(config.name), id);
        }

        // Reselect the currently configured parent, if any.
        const QString currentId =
            QString::fromStdString(boost::uuids::to_string(self->config_.config_id));
        const int idx = self->ui_->configCombo->findData(currentId);
        if (idx >= 0)
            self->ui_->configCombo->setCurrentIndex(idx);

        self->ui_->configCombo->blockSignals(false);

        BOOST_LOG_SEV(lg(), debug)
            << "Generation config combo populated with " << result.configs.size() << " entries";
    });

    watcher->setFuture(future);
}

void FxSpotGenerationConfigDetailDialog::updateUiFromConfig() {
    {
        const QString currentId =
            QString::fromStdString(boost::uuids::to_string(config_.config_id));
        const int idx = ui_->configCombo->findData(currentId);
        if (idx >= 0)
            ui_->configCombo->setCurrentIndex(idx);
    }
    ui_->sourceNameEdit->setText(QString::fromStdString(config_.source_name));
    ui_->oreKeyEdit->setText(QString::fromStdString(config_.ore_key));
    ui_->initialPriceSpin->setValue(config_.gmm_initial_price);
    ui_->ticksPerHourSpin->setValue(config_.ticks_per_hour);
    ui_->enabledCheck->setChecked(config_.enabled);

    populateProvenance(config_.version,
                       config_.modified_by,
                       config_.performed_by,
                       config_.recorded_at,
                       config_.change_reason_code,
                       config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::updateConfigFromUi() {
    const QString parentId = ui_->configCombo->currentData().toString();
    if (!parentId.isEmpty()) {
        config_.config_id = boost::lexical_cast<boost::uuids::uuid>(parentId.toStdString());
    }
    config_.source_name = ui_->sourceNameEdit->text().trimmed().toStdString();
    config_.ore_key = ui_->oreKeyEdit->text().trimmed().toStdString();
    config_.gmm_initial_price = ui_->initialPriceSpin->value();
    config_.ticks_per_hour = ui_->ticksPerHourSpin->value();
    config_.enabled = ui_->enabledCheck->isChecked();
    config_.modified_by = username_;
}

void FxSpotGenerationConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput();
    ui_->saveButton->setEnabled(canSave);
}

bool FxSpotGenerationConfigDetailDialog::validateInput() {
    const QString source_name_val = ui_->sourceNameEdit->text().trimmed();
    const QString ore_key_val = ui_->oreKeyEdit->text().trimmed();
    const bool hasParent = !ui_->configCombo->currentData().toString().isEmpty();
    return hasParent && !source_name_val.isEmpty() && !ore_key_val.isEmpty();
}

void FxSpotGenerationConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save FX spot generation config while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    config_.change_reason_code = crSel->reason_code;
    config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving FX spot generation config: " << config_.source_name;

    QPointer<FxSpotGenerationConfigDetailDialog> self = this;
    const bool wasCreate = createMode_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, config = config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        auto request = synthetic::messaging::save_fx_spot_generation_config_request::from(config);
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher, wasCreate]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "FX Spot Generation Config saved successfully";
            QString id = QString::fromStdString(boost::uuids::to_string(self->config_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            if (wasCreate) {
                emit self->fxSpotConfigCreated(id);
            } else {
                emit self->fxSpotConfigUpdated(id);
            }
            self->notifySaveSuccess(tr("FX Spot Generation Config '%1' saved")
                                        .arg(QString::fromStdString(self->config_.source_name)));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
