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
#include "ores.qt/PricingModelProductDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QComboBox>
#include <boost/uuid/random_generator.hpp>
#include "ui_PricingModelProductDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.analytics.api/messaging/pricing_model_product_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

PricingModelProductDetailDialog::PricingModelProductDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::PricingModelProductDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupCombos();
    setupConnections();
}

PricingModelProductDetailDialog::~PricingModelProductDetailDialog() {
    delete ui_;
}

QTabWidget* PricingModelProductDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* PricingModelProductDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* PricingModelProductDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void PricingModelProductDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void PricingModelProductDetailDialog::setupCombos() {
}

void PricingModelProductDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &PricingModelProductDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &PricingModelProductDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &PricingModelProductDetailDialog::onCloseClicked);

    connect(ui_->modelEdit, &QLineEdit::textChanged, this,
            &PricingModelProductDetailDialog::onFieldChanged);
    connect(ui_->engineEdit, &QLineEdit::textChanged, this,
            &PricingModelProductDetailDialog::onFieldChanged);
}

void PricingModelProductDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void PricingModelProductDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void PricingModelProductDetailDialog::setProduct(
    const analytics::domain::pricing_model_product& product) {
    product_ = product;
    updateUiFromProduct();
}

void PricingModelProductDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->pricingEngineTypeCodeCombo->setEnabled(createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        product_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelProductDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->pricingEngineTypeCodeCombo->setEnabled(false);
    ui_->modelEdit->setReadOnly(readOnly);
    ui_->engineEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void PricingModelProductDetailDialog::setPricingEngineTypes(
    const std::vector<analytics::domain::pricing_engine_type>& items) {
    pricingEngineTypes_ = items;
    populatePricingEngineTypes();
}

void PricingModelProductDetailDialog::populatePricingEngineTypes() {
    const auto current = ui_->pricingEngineTypeCodeCombo->currentData();
    ui_->pricingEngineTypeCodeCombo->clear();
    for (const auto& item : pricingEngineTypes_) {
        ui_->pricingEngineTypeCodeCombo->addItem(
            QString::fromStdString(item.code),
            QString::fromStdString(item.code));
    }
    if (current.isValid()) {
        const int idx = ui_->pricingEngineTypeCodeCombo->findData(current);
        if (idx >= 0) ui_->pricingEngineTypeCodeCombo->setCurrentIndex(idx);
    }
}

void PricingModelProductDetailDialog::updateUiFromProduct() {
    {
        const auto val = QString::fromStdString(product_.pricing_engine_type_code);
        const int idx = ui_->pricingEngineTypeCodeCombo->findData(val);
        if (idx >= 0) ui_->pricingEngineTypeCodeCombo->setCurrentIndex(idx);
    }
    ui_->modelEdit->setText(QString::fromStdString(product_.model));
    ui_->engineEdit->setText(QString::fromStdString(product_.engine));

    populateProvenance(product_.version,
                       product_.modified_by,
                       product_.performed_by,
                       product_.recorded_at,
                       product_.change_reason_code,
                       product_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void PricingModelProductDetailDialog::updateProductFromUi() {
    if (createMode_) {
    }
    product_.model = ui_->modelEdit->text().trimmed().toStdString();
    product_.engine = ui_->engineEdit->text().trimmed().toStdString();
    product_.modified_by = username_;
}

void PricingModelProductDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelProductDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void PricingModelProductDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool PricingModelProductDetailDialog::validateInput() {
    const QString model_val = ui_->modelEdit->text().trimmed();
    const QString engine_val = ui_->engineEdit->text().trimmed();
    const bool pricing_engine_type_code_selected = ui_->pricingEngineTypeCodeCombo->currentIndex() >= 0;

    return true
        && !model_val.isEmpty()
        && !engine_val.isEmpty()
        && pricing_engine_type_code_selected
    ;
}

void PricingModelProductDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save pricing model product while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateProductFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving pricing model product: "
        << product_.pricing_engine_type_code;

    QPointer<PricingModelProductDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, product = product_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::save_pricing_model_product_request request;
        request.data = product;
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
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Product saved successfully";
            QString code = QString::fromStdString(
                self->product_.pricing_engine_type_code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->productSaved(code);
            self->notifySaveSuccess(tr("Pricing Model Product '%1' saved").arg(code));
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

void PricingModelProductDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete pricing model product while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(
        product_.pricing_engine_type_code);
    auto reply = MessageBoxHelper::question(this, "Delete Pricing Model Product",
        QString("Are you sure you want to delete pricing model product '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting pricing model product: "
        << product_.pricing_engine_type_code;

    QPointer<PricingModelProductDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = product_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        analytics::messaging::delete_pricing_model_product_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Pricing Model Product deleted successfully";
            emit self->statusMessage(
                QString("Pricing Model Product '%1' deleted").arg(code));
            emit self->productDeleted(code);
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
