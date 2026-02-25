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
#include "ores.qt/TenantDetailDialog.hpp"

#include <QComboBox>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include "ui_TenantDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

TenantDetailDialog::TenantDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::TenantDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupConnections();
}

TenantDetailDialog::~TenantDetailDialog() {
    delete ui_;
}

QTabWidget* TenantDetailDialog::tabWidget() const { return ui_->tabWidget; }
QWidget* TenantDetailDialog::provenanceTab() const { return ui_->provenanceTab; }
ProvenanceWidget* TenantDetailDialog::provenanceWidget() const { return ui_->provenanceWidget; }

void TenantDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenantDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &TenantDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &TenantDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &TenantDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &TenantDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &TenantDetailDialog::onFieldChanged);
    connect(ui_->typeCombo, &QComboBox::currentTextChanged, this,
            &TenantDetailDialog::onFieldChanged);
    connect(ui_->hostnameEdit, &QLineEdit::textChanged, this,
            &TenantDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &TenantDetailDialog::onFieldChanged);
}

void TenantDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateLookups();
}

void TenantDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenantDetailDialog::populateLookups() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<TenantDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> lookup_result {
        return fetch_tenant_lookups(cm);
    };

    auto* watcher = new QFutureWatcher<lookup_result>(self);
    connect(watcher, &QFutureWatcher<lookup_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->ui_->typeCombo->clear();
        for (const auto& code : result.type_codes) {
            self->ui_->typeCombo->addItem(
                QString::fromStdString(code));
        }

        self->ui_->statusCombo->clear();
        for (const auto& code : result.status_codes) {
            self->ui_->statusCombo->addItem(
                QString::fromStdString(code));
        }

        self->updateUiFromTenant();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void TenantDetailDialog::setTenant(
    const iam::domain::tenant& tenant) {
    tenant_ = tenant;
    updateUiFromTenant();
}

void TenantDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    setProvenanceEnabled(!createMode);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenantDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->typeCombo->setEnabled(!readOnly);
    ui_->hostnameEdit->setReadOnly(readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenantDetailDialog::updateUiFromTenant() {
    ui_->codeEdit->setText(QString::fromStdString(tenant_.code));
    ui_->nameEdit->setText(QString::fromStdString(tenant_.name));
    ui_->typeCombo->setCurrentText(QString::fromStdString(tenant_.type));
    ui_->hostnameEdit->setText(QString::fromStdString(tenant_.hostname));
    ui_->statusCombo->setCurrentText(QString::fromStdString(tenant_.status));

    populateProvenance(tenant_.version, tenant_.modified_by,
        tenant_.performed_by, tenant_.recorded_at,
        tenant_.change_reason_code, tenant_.change_commentary);
}

void TenantDetailDialog::updateTenantFromUi() {
    if (createMode_) {
        tenant_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    tenant_.name = ui_->nameEdit->text().trimmed().toStdString();
    tenant_.type = ui_->typeCombo->currentText().trimmed().toStdString();
    tenant_.hostname = ui_->hostnameEdit->text().trimmed().toStdString();
    tenant_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    tenant_.modified_by = username_;
    tenant_.performed_by = username_;
}

void TenantDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenantDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenantDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenantDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return !code_val.isEmpty() && !name_val.isEmpty();
}

void TenantDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save tenant while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateTenantFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenant: " << tenant_.code;

    QPointer<TenantDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, tenant = tenant_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        iam::messaging::save_tenant_request request;
        request.tenant = tenant;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_tenant_request,
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

        auto response = iam::messaging::save_tenant_response::
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
            BOOST_LOG_SEV(lg(), info) << "Tenant saved successfully";
            QString code = QString::fromStdString(self->tenant_.code);
            emit self->tenantSaved(code);
            self->notifySaveSuccess(tr("Tenant '%1' saved").arg(code));
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

void TenantDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete tenant while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(tenant_.code);
    auto reply = MessageBoxHelper::question(this, "Delete Tenant",
        QString("Are you sure you want to delete tenant '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting tenant: " << tenant_.code;

    QPointer<TenantDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = tenant_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        iam::messaging::delete_tenant_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_tenant_request,
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

        auto response = iam::messaging::delete_tenant_response::
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
            BOOST_LOG_SEV(lg(), info) << "Tenant deleted successfully";
            emit self->statusMessage(QString("Tenant '%1' deleted").arg(code));
            emit self->tenantDeleted(code);
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
