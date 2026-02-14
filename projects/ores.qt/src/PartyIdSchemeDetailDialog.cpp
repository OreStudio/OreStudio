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
#include "ores.qt/PartyIdSchemeDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_PartyIdSchemeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/party_id_scheme_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

PartyIdSchemeDetailDialog::PartyIdSchemeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PartyIdSchemeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

PartyIdSchemeDetailDialog::~PartyIdSchemeDetailDialog() {
    delete ui_;
}

void PartyIdSchemeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void PartyIdSchemeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PartyIdSchemeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PartyIdSchemeDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &PartyIdSchemeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
            &PartyIdSchemeDetailDialog::onFieldChanged);
}

void PartyIdSchemeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PartyIdSchemeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PartyIdSchemeDetailDialog::setScheme(
    const refdata::domain::party_id_scheme& scheme) {
    scheme_ = scheme;
    updateUiFromScheme();
}

void PartyIdSchemeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PartyIdSchemeDetailDialog::updateUiFromScheme() {
    ui_->codeEdit->setText(QString::fromStdString(scheme_.code));
    ui_->nameEdit->setText(QString::fromStdString(scheme_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(scheme_.description));

    ui_->versionEdit->setText(QString::number(scheme_.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(scheme_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(scheme_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(scheme_.change_commentary));
}

void PartyIdSchemeDetailDialog::updateSchemeFromUi() {
    if (createMode_) {
        scheme_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    scheme_.name = ui_->nameEdit->text().trimmed().toStdString();
    scheme_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    scheme_.modified_by = username_;
    scheme_.performed_by = username_;
}

void PartyIdSchemeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PartyIdSchemeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PartyIdSchemeDetailDialog::validateInput() {
    const QString code = ui_->codeEdit->text().trimmed();
    const QString name = ui_->nameEdit->text().trimmed();

    return !code.isEmpty() && !name.isEmpty();
}

void PartyIdSchemeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save party ID scheme while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields (Code and Name).");
        return;
    }

    updateSchemeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving party ID scheme: " << scheme_.code;

    QPointer<PartyIdSchemeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, scheme = scheme_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_party_id_scheme_request request;
        request.scheme = scheme;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_party_id_scheme_request,
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

        auto response = refdata::messaging::save_party_id_scheme_response::
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
            BOOST_LOG_SEV(lg(), info) << "Party ID Scheme saved successfully";
            QString code = QString::fromStdString(self->scheme_.code);
            emit self->schemeSaved(code);
            self->notifySaveSuccess(tr("Party ID Scheme '%1' saved").arg(code));
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

void PartyIdSchemeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete party ID scheme while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(scheme_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Party ID Scheme",
        QString("Are you sure you want to delete party ID scheme '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting party ID scheme: " << scheme_.code;

    QPointer<PartyIdSchemeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = scheme_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_party_id_scheme_request request;
        request.codes = {code};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_party_id_scheme_request,
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

        auto response = refdata::messaging::delete_party_id_scheme_response::
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
            BOOST_LOG_SEV(lg(), info) << "Party ID Scheme deleted successfully";
            emit self->statusMessage(QString("Party ID Scheme '%1' deleted").arg(code));
            emit self->schemeDeleted(code);
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
