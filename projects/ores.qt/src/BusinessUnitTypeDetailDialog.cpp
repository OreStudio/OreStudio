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
#include "ores.qt/BusinessUnitTypeDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_BusinessUnitTypeDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata/messaging/business_unit_type_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

BusinessUnitTypeDetailDialog::BusinessUnitTypeDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BusinessUnitTypeDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

BusinessUnitTypeDetailDialog::~BusinessUnitTypeDetailDialog() {
    delete ui_;
}

QTabWidget* BusinessUnitTypeDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* BusinessUnitTypeDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* BusinessUnitTypeDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void BusinessUnitTypeDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void BusinessUnitTypeDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BusinessUnitTypeDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BusinessUnitTypeDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BusinessUnitTypeDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &BusinessUnitTypeDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
    connect(ui_->levelSpinBox, qOverload<int>(&QSpinBox::valueChanged), this,
            [this](int) { onFieldChanged(); });
    connect(ui_->codingSchemeEdit, &QLineEdit::textChanged, this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QTextEdit::textChanged, this,
            &BusinessUnitTypeDetailDialog::onFieldChanged);
}

void BusinessUnitTypeDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BusinessUnitTypeDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BusinessUnitTypeDetailDialog::setType(
    const refdata::domain::business_unit_type& type) {
    type_ = type;
    updateUiFromType();
}

void BusinessUnitTypeDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->levelSpinBox->setEnabled(!readOnly);
    ui_->codingSchemeEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BusinessUnitTypeDetailDialog::updateUiFromType() {
    ui_->codeEdit->setText(QString::fromStdString(type_.code));
    ui_->nameEdit->setText(QString::fromStdString(type_.name));
    ui_->levelSpinBox->setValue(type_.level);
    ui_->codingSchemeEdit->setText(QString::fromStdString(type_.coding_scheme_code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(type_.description));

    populateProvenance(type_.version, type_.modified_by,
                       type_.performed_by, type_.recorded_at,
                       type_.change_reason_code,
                       type_.change_commentary);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::updateTypeFromUi() {
    if (createMode_) {
        type_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    type_.name = ui_->nameEdit->text().trimmed().toStdString();
    type_.level = ui_->levelSpinBox->value();
    type_.coding_scheme_code = ui_->codingSchemeEdit->text().trimmed().toStdString();
    type_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    type_.modified_by = username_;
    type_.performed_by = username_;
}

void BusinessUnitTypeDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BusinessUnitTypeDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool BusinessUnitTypeDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void BusinessUnitTypeDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save business unit type while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTypeFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving business unit type: " << type_.code;

    QPointer<BusinessUnitTypeDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, type = type_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_business_unit_type_request request;
        request.types.push_back(type);
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_business_unit_type_request,
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

        auto response = refdata::messaging::save_business_unit_type_response::
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
            BOOST_LOG_SEV(lg(), info) << "Business Unit Type saved successfully";
            QString code = QString::fromStdString(self->type_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->typeSaved(code);
            self->notifySaveSuccess(tr("Business Unit Type '%1' saved").arg(code));
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

void BusinessUnitTypeDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete business unit type while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(type_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Business Unit Type",
        QString("Are you sure you want to delete business unit type '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting business unit type: " << type_.code;

    QPointer<BusinessUnitTypeDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = type_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_business_unit_type_request request;
        request.ids.push_back({id});
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_business_unit_type_request,
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

        auto response = refdata::messaging::delete_business_unit_type_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Business Unit Type deleted successfully";
            emit self->statusMessage(
                QString("Business Unit Type '%1' deleted").arg(code));
            emit self->typeDeleted(code);
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
