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
#include "ores.qt/PricingModelConfigDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/random_generator.hpp>
#include "ui_PricingModelConfigDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.analytics.api/messaging/pricing_model_config_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelConfigDetailDialog::PricingModelConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PricingModelConfigDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

PricingModelConfigDetailDialog::~PricingModelConfigDetailDialog() {
    delete ui_;
}

QTabWidget* PricingModelConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PricingModelConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PricingModelConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void PricingModelConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PricingModelConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PricingModelConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PricingModelConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &PricingModelConfigDetailDialog::onCloseClicked);

    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &PricingModelConfigDetailDialog::onCodeChanged);
    connect(ui_->configVariantEdit, &QLineEdit::textChanged, this,
            &PricingModelConfigDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &PricingModelConfigDetailDialog::onFieldChanged);
}

void PricingModelConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PricingModelConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PricingModelConfigDetailDialog::setConfig(
    const analytics::domain::pricing_model_config& config) {
    config_ = config;
    updateUiFromConfig();
}

void PricingModelConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->nameEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        config_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->nameEdit->setReadOnly(true);
    ui_->configVariantEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PricingModelConfigDetailDialog::updateUiFromConfig() {
    ui_->nameEdit->setText(QString::fromStdString(config_.name));
    ui_->configVariantEdit->setText(config_.config_variant
        ? QString::fromStdString(*config_.config_variant)
        : QString{});
    ui_->descriptionEdit->setPlainText(QString::fromStdString(config_.description));

    populateProvenance(config_.version,
                       config_.modified_by,
                       config_.performed_by,
                       config_.recorded_at,
                       config_.change_reason_code,
                       config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelConfigDetailDialog::updateConfigFromUi() {
    if (createMode_) {
        config_.name = ui_->nameEdit->text().trimmed().toStdString();
    }
    config_.config_variant = ui_->configVariantEdit->text().trimmed().toStdString();
    config_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    config_.modified_by = username_;
}

void PricingModelConfigDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PricingModelConfigDetailDialog::validateInput() {
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true
        && !name_val.isEmpty()
    ;
}

void PricingModelConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save pricing model configuration while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving pricing model configuration: "
        << config_.name;

    QPointer<PricingModelConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, config = config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::save_pricing_model_config_request request;
        request.data = config;
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
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration saved successfully";
            QString code = QString::fromStdString(
                self->config_.name);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->configSaved(code);
            self->notifySaveSuccess(tr("Pricing Model Configuration '%1' saved").arg(code));
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

void PricingModelConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete pricing model configuration while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        config_.name);
    auto reply = MessageBoxHelper::question(this, "Delete Pricing Model Configuration",
        QString("Are you sure you want to delete pricing model configuration '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting pricing model configuration: "
        << config_.name;

    QPointer<PricingModelConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = config_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::delete_pricing_model_config_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Configuration deleted successfully";
            emit self->statusMessage(
                QString("Pricing Model Configuration '%1' deleted").arg(code));
            emit self->configDeleted(code);
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
