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
#include "ores.qt/PricingModelProductParameterDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QComboBox>
#include <boost/uuid/random_generator.hpp>
#include "ui_PricingModelProductParameterDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_parameter_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelProductParameterDetailDialog::PricingModelProductParameterDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PricingModelProductParameterDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupCombos();
    setupConnections();
}

PricingModelProductParameterDetailDialog::~PricingModelProductParameterDetailDialog() {
    delete ui_;
}

QTabWidget* PricingModelProductParameterDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PricingModelProductParameterDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PricingModelProductParameterDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void PricingModelProductParameterDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PricingModelProductParameterDetailDialog::setupCombos() {
    ui_->parameterScopeCombo->clear();
    ui_->parameterScopeCombo->addItem(tr("Model"), QString("model"));
    ui_->parameterScopeCombo->addItem(tr("Engine"), QString("engine"));
    ui_->parameterScopeCombo->addItem(tr("Global"), QString("global"));
}

void PricingModelProductParameterDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PricingModelProductParameterDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PricingModelProductParameterDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &PricingModelProductParameterDetailDialog::onCloseClicked);

    connect(ui_->parameterScopeCombo, &QComboBox::currentIndexChanged, this,
            &PricingModelProductParameterDetailDialog::onFieldChanged);
    connect(ui_->parameterNameEdit, &QLineEdit::textChanged, this,
            &PricingModelProductParameterDetailDialog::onCodeChanged);
    connect(ui_->parameterValueEdit, &QLineEdit::textChanged, this,
            &PricingModelProductParameterDetailDialog::onFieldChanged);
}

void PricingModelProductParameterDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PricingModelProductParameterDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PricingModelProductParameterDetailDialog::setParameter(
    const analytics::domain::pricing_model_product_parameter& parameter) {
    parameter_ = parameter;
    updateUiFromParameter();
}

void PricingModelProductParameterDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->parameterNameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        parameter_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelProductParameterDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->parameterScopeCombo->setEnabled(!readOnly);
    ui_->parameterNameEdit->setReadOnly(true);
    ui_->parameterValueEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PricingModelProductParameterDetailDialog::updateUiFromParameter() {
    {
        const int idx = ui_->parameterScopeCombo->findData(
            QString::fromStdString(parameter_.parameter_scope));
        if (idx >= 0) ui_->parameterScopeCombo->setCurrentIndex(idx);
    }
    ui_->parameterNameEdit->setText(QString::fromStdString(parameter_.parameter_name));
    ui_->parameterValueEdit->setText(QString::fromStdString(parameter_.parameter_value));

    populateProvenance(parameter_.version,
                       parameter_.modified_by,
                       parameter_.performed_by,
                       parameter_.recorded_at,
                       parameter_.change_reason_code,
                       parameter_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelProductParameterDetailDialog::updateParameterFromUi() {
    parameter_.parameter_scope =
        ui_->parameterScopeCombo->currentData().toString().toStdString();
    if (createMode_) {
        parameter_.parameter_name = ui_->parameterNameEdit->text().trimmed().toStdString();
    }
    parameter_.parameter_value = ui_->parameterValueEdit->text().trimmed().toStdString();
    parameter_.modified_by = username_;
}

void PricingModelProductParameterDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelProductParameterDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelProductParameterDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PricingModelProductParameterDetailDialog::validateInput() {
    const QString parameter_name_val = ui_->parameterNameEdit->text().trimmed();
    const QString parameter_value_val = ui_->parameterValueEdit->text().trimmed();

    return true
        && !parameter_name_val.isEmpty()
        && !parameter_value_val.isEmpty()
    ;
}

void PricingModelProductParameterDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save pricing model product parameter while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateParameterFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving pricing model product parameter: "
        << parameter_.parameter_name;

    QPointer<PricingModelProductParameterDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, parameter = parameter_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::save_pricing_model_product_parameter_request request;
        request.data = parameter;
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter saved successfully";
            QString code = QString::fromStdString(
                self->parameter_.parameter_name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->parameterSaved(code);
            self->notifySaveSuccess(tr("Pricing Model Product Parameter '%1' saved").arg(code));
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

void PricingModelProductParameterDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete pricing model product parameter while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        parameter_.parameter_name);
    auto reply = MessageBoxHelper::question(this, "Delete Pricing Model Product Parameter",
        QString("Are you sure you want to delete pricing model product parameter '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting pricing model product parameter: "
        << parameter_.parameter_name;

    QPointer<PricingModelProductParameterDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = parameter_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::delete_pricing_model_product_parameter_request request;
        request.ids = {id};
        auto response_result = self->clientManager_->
            process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Product Parameter deleted successfully";
            emit self->statusMessage(
                QString("Pricing Model Product Parameter '%1' deleted").arg(code));
            emit self->parameterDeleted(code);
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
