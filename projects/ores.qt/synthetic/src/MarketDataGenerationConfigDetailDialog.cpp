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
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

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

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void MarketDataGenerationConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &MarketDataGenerationConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &MarketDataGenerationConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &MarketDataGenerationConfigDetailDialog::onCloseClicked);

    connect(ui_->nameEdit,
            &QLineEdit::textChanged,
            this,
            &MarketDataGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QLineEdit::textChanged,
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
    const synthetic::domain::market_data_generation_config& market_data_generation_config) {
    market_data_generation_config_ = market_data_generation_config;
    updateUiFromConfig();
}

void MarketDataGenerationConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        market_data_generation_config_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void MarketDataGenerationConfigDetailDialog::updateUiFromConfig() {
    ui_->nameEdit->setText(QString::fromStdString(market_data_generation_config_.name));
    ui_->descriptionEdit->setText(
        QString::fromStdString(market_data_generation_config_.description));
    ui_->enabledCheck->setChecked(market_data_generation_config_.enabled);

    populateProvenance(market_data_generation_config_.version,
                       market_data_generation_config_.modified_by,
                       market_data_generation_config_.performed_by,
                       market_data_generation_config_.recorded_at,
                       market_data_generation_config_.change_reason_code,
                       market_data_generation_config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::updateConfigFromUi() {
    market_data_generation_config_.name = ui_->nameEdit->text().trimmed().toStdString();
    market_data_generation_config_.description =
        ui_->descriptionEdit->text().trimmed().toStdString();
    market_data_generation_config_.enabled = ui_->enabledCheck->isChecked();
    market_data_generation_config_.modified_by = username_;
}

void MarketDataGenerationConfigDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void MarketDataGenerationConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool MarketDataGenerationConfigDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !name_val.isEmpty();
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
    market_data_generation_config_.change_reason_code = crSel->reason_code;
    market_data_generation_config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving market data generation config: "
                              << boost::uuids::to_string(market_data_generation_config_.id);

    QPointer<MarketDataGenerationConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 market_data_generation_config = market_data_generation_config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_market_data_generation_config_request request;
        request.data = market_data_generation_config;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Market Data Generation Config saved successfully";
            QString code = QString::fromStdString(
                boost::uuids::to_string(self->market_data_generation_config_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->market_data_generation_configSaved(code);
            self->notifySaveSuccess(tr("Market Data Generation Config '%1' saved").arg(code));
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

void MarketDataGenerationConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete market data generation config while disconnected from server.");
        return;
    }

    QString code =
        QString::fromStdString(boost::uuids::to_string(market_data_generation_config_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Market Data Generation Config",
        QString("Are you sure you want to delete market data generation config '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel = promptChangeReason(ChangeReasonDialog::OperationType::Delete, false);
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting market data generation config: "
                              << boost::uuids::to_string(market_data_generation_config_.id);

    QPointer<MarketDataGenerationConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self,
                 id_str =
                     boost::uuids::to_string(market_data_generation_config_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_market_data_generation_config_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Market Data Generation Config deleted successfully";
            emit self->statusMessage(
                QString("Market Data Generation Config '%1' deleted").arg(code));
            emit self->market_data_generation_configDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
