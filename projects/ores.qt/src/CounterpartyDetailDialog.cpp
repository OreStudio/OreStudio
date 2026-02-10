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
#include "ores.qt/CounterpartyDetailDialog.hpp"

#include <QComboBox>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_CounterpartyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CounterpartyDetailDialog::CounterpartyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CounterpartyDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CounterpartyDetailDialog::~CounterpartyDetailDialog() {
    delete ui_;
}

void CounterpartyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void CounterpartyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CounterpartyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CounterpartyDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->partyTypeCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->businessCenterEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
}

void CounterpartyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateLookups();
}

void CounterpartyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CounterpartyDetailDialog::populateLookups() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<CounterpartyDetailDialog> self = this;

    auto task = [self]() -> lookup_result {
        if (!self || !self->clientManager_) return {};
        return fetch_party_lookups(self->clientManager_);
    };

    auto* watcher = new QFutureWatcher<lookup_result>(self);
    connect(watcher, &QFutureWatcher<lookup_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->ui_->partyTypeCombo->clear();
        for (const auto& code : result.type_codes) {
            self->ui_->partyTypeCombo->addItem(
                QString::fromStdString(code));
        }

        self->ui_->statusCombo->clear();
        for (const auto& code : result.status_codes) {
            self->ui_->statusCombo->addItem(
                QString::fromStdString(code));
        }

        self->updateUiFromCounterparty();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::setCounterparty(
    const refdata::domain::counterparty& counterparty) {
    counterparty_ = counterparty;
    updateUiFromCounterparty();
}

void CounterpartyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->partyTypeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->businessCenterEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CounterpartyDetailDialog::updateUiFromCounterparty() {
    ui_->codeEdit->setText(QString::fromStdString(counterparty_.short_code));
    ui_->nameEdit->setText(QString::fromStdString(counterparty_.full_name));
    ui_->partyTypeCombo->setCurrentText(QString::fromStdString(counterparty_.party_type));
    ui_->statusCombo->setCurrentText(QString::fromStdString(counterparty_.status));
    ui_->businessCenterEdit->setText(QString::fromStdString(counterparty_.business_center_code));

    ui_->versionEdit->setText(QString::number(counterparty_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(counterparty_.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(counterparty_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(counterparty_.change_commentary));
}

void CounterpartyDetailDialog::updateCounterpartyFromUi() {
    if (createMode_) {
        counterparty_.short_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    counterparty_.full_name = ui_->nameEdit->text().trimmed().toStdString();
    counterparty_.party_type = ui_->partyTypeCombo->currentText().trimmed().toStdString();
    counterparty_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    counterparty_.business_center_code = ui_->businessCenterEdit->text().trimmed().toStdString();
    counterparty_.recorded_by = username_;
    counterparty_.performed_by = username_;
}

void CounterpartyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CounterpartyDetailDialog::validateInput() {
    const QString short_code_val = ui_->codeEdit->text().trimmed();
    const QString full_name_val = ui_->nameEdit->text().trimmed();

    return !short_code_val.isEmpty() && !full_name_val.isEmpty();
}

void CounterpartyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save counterparty while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateCounterpartyFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving counterparty: " << counterparty_.short_code;

    QPointer<CounterpartyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, counterparty = counterparty_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_counterparty_request request;
        request.counterparty = counterparty;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_counterparty_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::save_counterparty_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Counterparty saved successfully";
            QString code = QString::fromStdString(self->counterparty_.short_code);
            emit self->counterpartySaved(code);
            self->notifySaveSuccess(tr("Counterparty '%1' saved").arg(code));
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

void CounterpartyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete counterparty while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(counterparty_.short_code);
    auto reply = MessageBoxHelper::question(this, "Delete Counterparty",
        QString("Are you sure you want to delete counterparty '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting counterparty: " << counterparty_.short_code;

    QPointer<CounterpartyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = counterparty_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_counterparty_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_counterparty_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::delete_counterparty_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Counterparty deleted successfully";
            emit self->statusMessage(QString("Counterparty '%1' deleted").arg(code));
            emit self->counterpartyDeleted(code);
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
