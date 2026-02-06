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

#include <QtConcurrent>
#include <QFutureWatcher>
#include <QVBoxLayout>
#include <QToolBar>
#include <boost/uuid/uuid_io.hpp>
#include "ui_TenantDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/MdiUtils.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.qt/ExceptionHelper.hpp"
#include "ores.iam/messaging/tenant_protocol.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using comms::messaging::frame;
using comms::messaging::message_type;
using namespace ores::logging;

namespace {

struct SaveResult {
    bool success;
    QString error_message;
    QString error_details;
};

}

TenantDetailDialog::TenantDetailDialog(QWidget* parent)
    : DetailDialogBase(parent), ui_(new Ui::TenantDetailDialog), isDirty_(false),
      isAddMode_(false), isReadOnly_(false), isStale_(false),
      historicalVersion_(0), clientManager_(nullptr) {

    ui_->setupUi(this);

    setupToolbar();
    populateTypeCombo();
    populateStatusCombo();

    // Connect signals for editable fields to detect changes
    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
        &TenantDetailDialog::onFieldChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
        &TenantDetailDialog::onFieldChanged);
    connect(ui_->hostnameEdit, &QLineEdit::textChanged, this,
        &TenantDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
        &TenantDetailDialog::onFieldChanged);
    connect(ui_->typeCombo, &QComboBox::currentIndexChanged, this,
        &TenantDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentIndexChanged, this,
        &TenantDetailDialog::onFieldChanged);

    updateSaveButtonState();
}

TenantDetailDialog::~TenantDetailDialog() {
    const auto watchers = findChildren<QFutureWatcherBase*>();
    for (auto* watcher : watchers) {
        disconnect(watcher, nullptr, this, nullptr);
        watcher->cancel();
        watcher->waitForFinished();
    }
}

void TenantDetailDialog::setupToolbar() {
    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    // Create Save action
    saveAction_ = new QAction("Save", this);
    saveAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Save, IconUtils::DefaultIconColor));
    saveAction_->setToolTip("Save changes");
    connect(saveAction_, &QAction::triggered, this,
        &TenantDetailDialog::onSaveClicked);
    toolBar_->addAction(saveAction_);

    // Create Delete action
    deleteAction_ = new QAction("Delete", this);
    deleteAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::Delete, IconUtils::DefaultIconColor));
    deleteAction_->setToolTip("Delete tenant");
    connect(deleteAction_, &QAction::triggered, this,
        &TenantDetailDialog::onDeleteClicked);
    toolBar_->addAction(deleteAction_);

    // Create Revert action (initially hidden)
    revertAction_ = new QAction("Revert to this version", this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(
        Icon::ArrowClockwise, IconUtils::DefaultIconColor));
    revertAction_->setToolTip("Revert tenant to this historical version");
    connect(revertAction_, &QAction::triggered, this,
        &TenantDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    // Add toolbar to the dialog's layout
    auto* mainLayout = qobject_cast<QVBoxLayout*>(layout());
    if (mainLayout)
        mainLayout->insertWidget(0, toolBar_);
}

void TenantDetailDialog::populateTypeCombo() {
    // Common tenant types - these should ideally come from the server
    ui_->typeCombo->addItem("organization", "organization");
    ui_->typeCombo->addItem("system", "system");
    ui_->typeCombo->addItem("demo", "demo");
    ui_->typeCombo->addItem("test", "test");
}

void TenantDetailDialog::populateStatusCombo() {
    // Common tenant statuses - these should ideally come from the server
    ui_->statusCombo->addItem("active", "active");
    ui_->statusCombo->addItem("suspended", "suspended");
    ui_->statusCombo->addItem("pending", "pending");
    ui_->statusCombo->addItem("archived", "archived");
}

void TenantDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void TenantDetailDialog::setUsername(const std::string& username) {
    modifiedByUsername_ = username;
}

void TenantDetailDialog::setTenant(const iam::domain::tenant& tenant) {
    currentTenant_ = tenant;
    isAddMode_ = tenant.code.empty();

    setCreateMode(isAddMode_);

    ui_->codeEdit->setText(QString::fromStdString(tenant.code));
    ui_->nameEdit->setText(QString::fromStdString(tenant.name));
    ui_->hostnameEdit->setText(QString::fromStdString(tenant.hostname));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(tenant.description));

    // Set type combo
    int typeIndex = ui_->typeCombo->findData(QString::fromStdString(tenant.type));
    if (typeIndex >= 0) {
        ui_->typeCombo->setCurrentIndex(typeIndex);
    } else if (!tenant.type.empty()) {
        // Add custom type if not in list
        ui_->typeCombo->addItem(QString::fromStdString(tenant.type),
            QString::fromStdString(tenant.type));
        ui_->typeCombo->setCurrentIndex(ui_->typeCombo->count() - 1);
    }

    // Set status combo
    int statusIndex = ui_->statusCombo->findData(QString::fromStdString(tenant.status));
    if (statusIndex >= 0) {
        ui_->statusCombo->setCurrentIndex(statusIndex);
    } else if (!tenant.status.empty()) {
        // Add custom status if not in list
        ui_->statusCombo->addItem(QString::fromStdString(tenant.status),
            QString::fromStdString(tenant.status));
        ui_->statusCombo->setCurrentIndex(ui_->statusCombo->count() - 1);
    }

    // Metadata fields
    ui_->versionEdit->setText(QString::number(tenant.version));
    ui_->recordedByEdit->setText(QString::fromStdString(tenant.recorded_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(tenant.recorded_at));
    ui_->changeReasonEdit->setText(QString::fromStdString(tenant.change_reason_code));
    ui_->changeCommentaryEdit->setPlainText(QString::fromStdString(tenant.change_commentary));

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void TenantDetailDialog::setCreateMode(bool createMode) {
    isAddMode_ = createMode;

    // Code is editable only in create mode
    ui_->codeEdit->setReadOnly(!createMode);

    // Metadata is not useful in create mode
    ui_->metadataGroup->setVisible(!createMode);
}

iam::domain::tenant TenantDetailDialog::getTenant() const {
    iam::domain::tenant tenant = currentTenant_;
    tenant.code = ui_->codeEdit->text().toStdString();
    tenant.name = ui_->nameEdit->text().toStdString();
    tenant.hostname = ui_->hostnameEdit->text().toStdString();
    tenant.description = ui_->descriptionEdit->toPlainText().toStdString();
    tenant.type = ui_->typeCombo->currentData().toString().toStdString();
    tenant.status = ui_->statusCombo->currentData().toString().toStdString();
    tenant.recorded_by = modifiedByUsername_.empty() ? "qt_user" : modifiedByUsername_;

    return tenant;
}

void TenantDetailDialog::clearDialog() {
    ui_->codeEdit->clear();
    ui_->nameEdit->clear();
    ui_->hostnameEdit->clear();
    ui_->descriptionEdit->clear();
    ui_->typeCombo->setCurrentIndex(0);
    ui_->statusCombo->setCurrentIndex(0);
    ui_->versionEdit->clear();
    ui_->recordedByEdit->clear();
    ui_->recordedAtEdit->clear();
    ui_->changeReasonEdit->clear();
    ui_->changeCommentaryEdit->clear();

    isDirty_ = false;
    emit isDirtyChanged(false);
    updateSaveButtonState();
}

void TenantDetailDialog::save() {
    onSaveClicked();
}

void TenantDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Save clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    // Validate required fields
    if (ui_->codeEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: code is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Tenant code is required.");
        return;
    }

    if (ui_->nameEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: name is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Tenant name is required.");
        return;
    }

    if (ui_->hostnameEdit->text().trimmed().isEmpty()) {
        BOOST_LOG_SEV(lg(), warn) << "Validation failed: hostname is empty";
        MessageBoxHelper::warning(this, "Validation Error",
            "Tenant hostname is required.");
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Save clicked for tenant: "
                               << currentTenant_.code;

    QPointer<TenantDetailDialog> self = this;
    const auto tenant = getTenant();
    const QString code = QString::fromStdString(tenant.code);

    QFuture<SaveResult> future = QtConcurrent::run([self, tenant]() -> SaveResult {
        return exception_helper::wrap_async_fetch<SaveResult>([&]() -> SaveResult {
            if (!self || !self->clientManager_) {
                return {.success = false,
                        .error_message = "Dialog closed",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Sending save tenant request for: "
                                       << tenant.code;

            iam::messaging::save_tenant_request request;
            request.tenant = tenant;

            auto payload = request.serialize();
            frame request_frame(message_type::save_tenant_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                return {.success = false,
                        .error_message = "Failed to communicate with server",
                        .error_details = {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                return {.success = false,
                        .error_message = "Failed to decompress server response",
                        .error_details = {}};
            }

            auto response = iam::messaging::save_tenant_response::
                deserialize(*payload_result);

            if (!response) {
                return {.success = false,
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            if (!response->success) {
                return {.success = false,
                        .error_message = QString::fromStdString(response->message),
                        .error_details = {}};
            }

            return {.success = true, .error_message = {}, .error_details = {}};
        }, "tenant");
    });

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher, code]() {
        if (!self) return;

        const auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), debug) << "Tenant saved successfully";

            self->isDirty_ = false;
            emit self->isDirtyChanged(false);
            self->updateSaveButtonState();

            emit self->tenantSaved(code);

            self->notifySaveSuccess(tr("Tenant '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Tenant save failed: "
                                       << result.error_message.toStdString();
            emit self->errorMessage(result.error_message);
            MessageBoxHelper::critical(self, tr("Save Error"),
                result.error_message, result.error_details);
        }
    });

    watcher->setFuture(future);
}

void TenantDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked but client not connected.";
        emit errorMessage("Not connected to server. Please login.");
        return;
    }

    if (isAddMode_) {
        BOOST_LOG_SEV(lg(), warn) << "Delete clicked in add mode.";
        return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Delete request for tenant: "
                               << currentTenant_.code;

    auto reply = MessageBoxHelper::question(this, "Delete Tenant",
        QString("Are you sure you want to delete tenant '%1'?")
            .arg(QString::fromStdString(currentTenant_.code)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Delete cancelled by user";
        return;
    }

    QPointer<TenantDetailDialog> self = this;
    const boost::uuids::uuid tenant_id = currentTenant_.id;
    const QString code = QString::fromStdString(currentTenant_.code);

    QFuture<SaveResult> future = QtConcurrent::run([self, tenant_id]() -> SaveResult {
        return exception_helper::wrap_async_fetch<SaveResult>([&]() -> SaveResult {
            if (!self || !self->clientManager_) {
                return {.success = false,
                        .error_message = "Dialog closed",
                        .error_details = {}};
            }

            BOOST_LOG_SEV(lg(), debug) << "Sending delete tenant request for: "
                                       << boost::uuids::to_string(tenant_id);

            iam::messaging::delete_tenant_request request;
            request.ids.push_back(tenant_id);

            auto payload = request.serialize();
            frame request_frame(message_type::delete_tenant_request,
                0, std::move(payload));

            auto response_result =
                self->clientManager_->sendRequest(std::move(request_frame));

            if (!response_result) {
                return {.success = false,
                        .error_message = "Failed to communicate with server",
                        .error_details = {}};
            }

            auto payload_result = response_result->decompressed_payload();
            if (!payload_result) {
                return {.success = false,
                        .error_message = "Failed to decompress server response",
                        .error_details = {}};
            }

            auto response = iam::messaging::delete_tenant_response::
                deserialize(*payload_result);

            if (!response) {
                return {.success = false,
                        .error_message = "Invalid server response",
                        .error_details = {}};
            }

            // Check results
            if (response->results.empty()) {
                return {.success = false,
                        .error_message = "No result returned from server",
                        .error_details = {}};
            }

            const auto& result = response->results[0];
            if (!result.success) {
                return {.success = false,
                        .error_message = QString::fromStdString(result.message),
                        .error_details = {}};
            }

            return {.success = true, .error_message = {}, .error_details = {}};
        }, "tenant");
    });

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher, code]() {
        if (!self) return;

        const auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), debug) << "Tenant deleted successfully.";
            emit self->statusMessage(QString("Successfully deleted tenant: %1").arg(code));
            emit self->tenantDeleted(code);

            // Close window after successful deletion
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Tenant deletion failed: "
                                       << result.error_message.toStdString();
            emit self->errorMessage(QString("Failed to delete tenant: %1")
                .arg(result.error_message));
            MessageBoxHelper::critical(self, "Delete Failed",
                result.error_message);
        }
    });

    watcher->setFuture(future);
}

void TenantDetailDialog::onRevertClicked() {
    BOOST_LOG_SEV(lg(), info) << "Revert clicked for historical version "
                              << historicalVersion_;

    // Confirm with user
    auto reply = MessageBoxHelper::question(this, "Revert Tenant",
        QString("Are you sure you want to revert '%1' to version %2?\n\n"
                "This will create a new version with the data from version %2.")
            .arg(QString::fromStdString(currentTenant_.code))
            .arg(historicalVersion_),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        BOOST_LOG_SEV(lg(), debug) << "Revert cancelled by user";
        return;
    }

    emit revertRequested(currentTenant_);
}

void TenantDetailDialog::onFieldChanged() {
    if (isReadOnly_)
        return;

    isDirty_ = true;
    emit isDirtyChanged(true);
    updateSaveButtonState();
}

void TenantDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    isReadOnly_ = readOnly;
    historicalVersion_ = versionNumber;

    BOOST_LOG_SEV(lg(), debug) << "Setting read-only mode: " << readOnly
                               << ", version: " << versionNumber;

    setFieldsReadOnly(readOnly);

    // Update toolbar visibility
    if (saveAction_)
        saveAction_->setVisible(!readOnly);

    if (deleteAction_)
        deleteAction_->setVisible(!readOnly);

    if (revertAction_)
        revertAction_->setVisible(readOnly);

    updateSaveButtonState();
}

void TenantDetailDialog::setFieldsReadOnly(bool readOnly) {
    ui_->codeEdit->setReadOnly(readOnly);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->hostnameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->typeCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
}

void TenantDetailDialog::updateSaveButtonState() {
    if (isReadOnly_) {
        if (saveAction_)
            saveAction_->setEnabled(false);
        if (deleteAction_)
            deleteAction_->setEnabled(false);
        if (revertAction_)
            revertAction_->setEnabled(true);
        return;
    }

    if (saveAction_)
        saveAction_->setEnabled(isDirty_);

    if (deleteAction_)
        deleteAction_->setEnabled(!isAddMode_);
}

void TenantDetailDialog::markAsStale() {
    if (isStale_)
        return;

    isStale_ = true;
    BOOST_LOG_SEV(lg(), info) << "Tenant detail data marked as stale for: "
                              << currentTenant_.code;

    MdiUtils::markParentWindowAsStale(this);

    emit statusMessage(QString("Tenant %1 has been modified on the server")
        .arg(QString::fromStdString(currentTenant_.code)));
}

QString TenantDetailDialog::tenantCode() const {
    return QString::fromStdString(currentTenant_.code);
}

}
