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
#include "ores.qt/PartyDetailDialog.hpp"

#include <QComboBox>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_PartyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/party_protocol.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

PartyDetailDialog::PartyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PartyDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

PartyDetailDialog::~PartyDetailDialog() {
    delete ui_;
}

void PartyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->partyCategoryCombo->addItem(
        QString::fromUtf8(party_categories::operational));
    ui_->partyCategoryCombo->addItem(
        QString::fromUtf8(party_categories::system));
}

void PartyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PartyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PartyDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &PartyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->partyCategoryCombo, &QComboBox::currentTextChanged, this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->partyTypeCombo, &QComboBox::currentTextChanged, this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &PartyDetailDialog::onFieldChanged);
    connect(ui_->businessCenterEdit, &QLineEdit::textChanged, this,
            &PartyDetailDialog::onFieldChanged);
}

void PartyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateLookups();
}

void PartyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyDetailDialog::populateLookups() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<PartyDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> lookup_result {
        return fetch_party_lookups(cm);
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

        self->updateUiFromParty();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void PartyDetailDialog::setParty(
    const refdata::domain::party& party) {
    party_ = party;
    updateUiFromParty();
}

void PartyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->partyCategoryCombo->setEnabled(!readOnly);
    ui_->partyTypeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->businessCenterEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PartyDetailDialog::updateUiFromParty() {
    ui_->codeEdit->setText(QString::fromStdString(party_.short_code));
    ui_->nameEdit->setText(QString::fromStdString(party_.full_name));
    ui_->partyCategoryCombo->setCurrentText(QString::fromStdString(party_.party_category));
    ui_->partyTypeCombo->setCurrentText(QString::fromStdString(party_.party_type));
    ui_->statusCombo->setCurrentText(QString::fromStdString(party_.status));
    ui_->businessCenterEdit->setText(QString::fromStdString(party_.business_center_code));

    ui_->versionEdit->setText(QString::number(party_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(party_.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(party_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(party_.change_commentary));
}

void PartyDetailDialog::updatePartyFromUi() {
    if (createMode_) {
        party_.short_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    party_.full_name = ui_->nameEdit->text().trimmed().toStdString();
    party_.party_category = ui_->partyCategoryCombo->currentText().trimmed().toStdString();
    party_.party_type = ui_->partyTypeCombo->currentText().trimmed().toStdString();
    party_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    const auto bcc = ui_->businessCenterEdit->text().trimmed().toStdString();
    party_.business_center_code = bcc.empty() ? std::string("WRLD") : bcc;
    party_.recorded_by = username_;
    party_.performed_by = username_;
}

void PartyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyDetailDialog::validateInput() {
    const QString short_code_val = ui_->codeEdit->text().trimmed();
    const QString full_name_val = ui_->nameEdit->text().trimmed();

    return !short_code_val.isEmpty() && !full_name_val.isEmpty();
}

void PartyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save party while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updatePartyFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party: " << party_.short_code;

    QPointer<PartyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, party = party_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_party_request request;
        request.party = party;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_party_request,
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

        auto response = refdata::messaging::save_party_response::
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
            BOOST_LOG_SEV(lg(), info) << "Party saved successfully";
            QString code = QString::fromStdString(self->party_.short_code);
            emit self->partySaved(code);
            self->notifySaveSuccess(tr("Party '%1' saved").arg(code));
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

void PartyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete party while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(party_.short_code);
    auto reply = MessageBoxHelper::question(this, "Delete Party",
        QString("Are you sure you want to delete party '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting party: " << party_.short_code;

    QPointer<PartyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = party_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_party_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_party_request,
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

        auto response = refdata::messaging::delete_party_response::
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
            BOOST_LOG_SEV(lg(), info) << "Party deleted successfully";
            emit self->statusMessage(QString("Party '%1' deleted").arg(code));
            emit self->partyDeleted(code);
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
