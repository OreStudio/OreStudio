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
#include "ores.qt/BusinessUnitDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_BusinessUnitDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/business_unit_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessUnitDetailDialog::BusinessUnitDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BusinessUnitDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BusinessUnitDetailDialog::~BusinessUnitDetailDialog() {
    delete ui_;
}

void BusinessUnitDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void BusinessUnitDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BusinessUnitDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BusinessUnitDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BusinessUnitDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BusinessUnitDetailDialog::onFieldChanged);
    connect(ui_->businessCentreEdit, &QLineEdit::textChanged, this,
            &BusinessUnitDetailDialog::onFieldChanged);
}

void BusinessUnitDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BusinessUnitDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessUnitDetailDialog::setUnit(
    const refdata::domain::business_unit& business_unit) {
    business_unit_ = business_unit;
    updateUiFromUnit();
}

void BusinessUnitDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        ui_->metadataGroup->setVisible(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->businessCentreEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessUnitDetailDialog::updateUiFromUnit() {
    ui_->codeEdit->setText(QString::fromStdString(business_unit_.unit_code));
    ui_->nameEdit->setText(QString::fromStdString(business_unit_.unit_name));
    ui_->businessCentreEdit->setText(QString::fromStdString(business_unit_.business_centre_code));

    ui_->versionEdit->setText(QString::number(business_unit_.version));
    ui_->recordedByEdit->setText(QString::fromStdString(business_unit_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(business_unit_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(business_unit_.change_commentary));
}

void BusinessUnitDetailDialog::updateUnitFromUi() {
    if (createMode_) {
        business_unit_.unit_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    business_unit_.unit_name = ui_->nameEdit->text().trimmed().toStdString();
    business_unit_.business_centre_code = ui_->businessCentreEdit->text().trimmed().toStdString();
    business_unit_.modified_by = username_;
    business_unit_.performed_by = username_;
}

void BusinessUnitDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessUnitDetailDialog::validateInput() {
    const QString unit_name_val = ui_->nameEdit->text().trimmed();

    return !unit_name_val.isEmpty();
}

void BusinessUnitDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save business unit while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateUnitFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business unit: " << business_unit_.unit_code;

    QPointer<BusinessUnitDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, business_unit = business_unit_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_unit_request request;
        request.business_unit = business_unit;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_business_unit_request,
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

        auto response = refdata::messaging::save_business_unit_response::
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
            BOOST_LOG_SEV(lg(), info) << "Business Unit saved successfully";
            QString code = QString::fromStdString(self->business_unit_.unit_code);
            emit self->business_unitSaved(code);
            self->notifySaveSuccess(tr("Business Unit '%1' saved").arg(code));
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

void BusinessUnitDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete business unit while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(business_unit_.unit_code);
    auto reply = MessageBoxHelper::question(this, "Delete Business Unit",
        QString("Are you sure you want to delete business unit '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting business unit: " << business_unit_.unit_code;

    QPointer<BusinessUnitDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = business_unit_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_unit_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_business_unit_request,
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

        auto response = refdata::messaging::delete_business_unit_response::
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
            BOOST_LOG_SEV(lg(), info) << "Business Unit deleted successfully";
            emit self->statusMessage(QString("Business Unit '%1' deleted").arg(code));
            emit self->business_unitDeleted(code);
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
