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
#include "ores.qt/MarketDataGenerationConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/market_data_generation_config_protocol.hpp"
#include "ui_MarketDataGenerationConfigDetailDialog.h"
#include <boost/uuid/uuid_io.hpp>
#include <QCheckBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

MarketDataGenerationConfigDetailDialog::MarketDataGenerationConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::MarketDataGenerationConfigDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

MarketDataGenerationConfigDetailDialog::~MarketDataGenerationConfigDetailDialog() {
    delete ui_;
}

QTabWidget* MarketDataGenerationConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* MarketDataGenerationConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* MarketDataGenerationConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void MarketDataGenerationConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void MarketDataGenerationConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &MarketDataGenerationConfigDetailDialog::onSaveClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &MarketDataGenerationConfigDetailDialog::onCloseClicked);

    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &MarketDataGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &MarketDataGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->enabledCheck,
            &QCheckBox::toggled,
            this,
            &MarketDataGenerationConfigDetailDialog::onFieldChanged);
}

void MarketDataGenerationConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void MarketDataGenerationConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void MarketDataGenerationConfigDetailDialog::setConfig(
    const synthetic::domain::market_data_generation_config& config) {
    config_ = config;
    updateUiFromConfig();
}

void MarketDataGenerationConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::updateUiFromConfig() {
    ui_->nameEdit->setText(QString::fromStdString(config_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(config_.description));
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

void MarketDataGenerationConfigDetailDialog::updateConfigFromUi() {
    config_.name = ui_->nameEdit->text().trimmed().toStdString();
    config_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    config_.enabled = ui_->enabledCheck->isChecked();
    config_.modified_by = username_;
}

void MarketDataGenerationConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput();
    ui_->saveButton->setEnabled(canSave);
}

bool MarketDataGenerationConfigDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();
    return !name_val.isEmpty();
}

void MarketDataGenerationConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save market data generation config while disconnected from server.");
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

    BOOST_LOG_SEV(lg(), info) << "Saving market data generation config: " << config_.name;

    QPointer<MarketDataGenerationConfigDetailDialog> self = this;
    const bool wasCreate = createMode_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, config = config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        auto request =
            synthetic::messaging::save_market_data_generation_config_request::from(config);
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
            BOOST_LOG_SEV(lg(), info) << "Market Data Generation Config saved successfully";
            QString id = QString::fromStdString(boost::uuids::to_string(self->config_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            if (wasCreate) {
                emit self->configCreated(id);
            } else {
                emit self->configUpdated(id);
            }
            self->notifySaveSuccess(
                tr("Market Data Generation Config '%1' saved")
                    .arg(QString::fromStdString(self->config_.name)));
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
