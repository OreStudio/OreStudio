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
#include "ores.qt/FxSpotGenerationConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.synthetic.api/messaging/fx_spot_generation_config_protocol.hpp"
#include "ui_FxSpotGenerationConfigDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxSpotGenerationConfigDetailDialog::FxSpotGenerationConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::FxSpotGenerationConfigDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
}

FxSpotGenerationConfigDetailDialog::~FxSpotGenerationConfigDetailDialog() {
    delete ui_;
}

QTabWidget* FxSpotGenerationConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FxSpotGenerationConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FxSpotGenerationConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FxSpotGenerationConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FxSpotGenerationConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &FxSpotGenerationConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &FxSpotGenerationConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &FxSpotGenerationConfigDetailDialog::onCloseClicked);

    connect(ui_->baseCurrencyEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->quoteCurrencyEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->priceSourceEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->gmmInitialPriceEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->processTypeEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->vintageSourceEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
    connect(ui_->vintageDateEdit,
            &QLineEdit::textChanged,
            this,
            &FxSpotGenerationConfigDetailDialog::onFieldChanged);
}

void FxSpotGenerationConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FxSpotGenerationConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FxSpotGenerationConfigDetailDialog::setConfig(
    const synthetic::domain::fx_spot_generation_config& fx_spot_generation_config) {
    fx_spot_generation_config_ = fx_spot_generation_config;
    updateUiFromConfig();
}

void FxSpotGenerationConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        fx_spot_generation_config_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->baseCurrencyEdit->setReadOnly(readOnly);
    ui_->quoteCurrencyEdit->setReadOnly(readOnly);
    ui_->priceSourceEdit->setReadOnly(readOnly);
    ui_->gmmInitialPriceEdit->setReadOnly(readOnly);
    ui_->processTypeEdit->setReadOnly(readOnly);
    ui_->vintageSourceEdit->setReadOnly(readOnly);
    ui_->vintageDateEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void FxSpotGenerationConfigDetailDialog::updateUiFromConfig() {
    ui_->baseCurrencyEdit->setText(
        QString::fromStdString(fx_spot_generation_config_.base_currency_code));
    ui_->quoteCurrencyEdit->setText(
        QString::fromStdString(fx_spot_generation_config_.quote_currency_code));
    ui_->priceSourceEdit->setText(QString::fromStdString(fx_spot_generation_config_.price_source));
    ui_->gmmInitialPriceEdit->setText(
        QString::number(fx_spot_generation_config_.gmm_initial_price));
    ui_->ticksPerHourEdit->setValue(fx_spot_generation_config_.ticks_per_hour);
    ui_->processTypeEdit->setText(QString::fromStdString(fx_spot_generation_config_.process_type));
    ui_->enabledCheck->setChecked(fx_spot_generation_config_.enabled);
    ui_->vintageSourceEdit->setText(
        QString::fromStdString(fx_spot_generation_config_.vintage_source));
    ui_->vintageDateEdit->setText(QString::fromStdString(fx_spot_generation_config_.vintage_date));

    populateProvenance(fx_spot_generation_config_.version,
                       fx_spot_generation_config_.modified_by,
                       fx_spot_generation_config_.performed_by,
                       fx_spot_generation_config_.recorded_at,
                       fx_spot_generation_config_.change_reason_code,
                       fx_spot_generation_config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::updateConfigFromUi() {
    fx_spot_generation_config_.base_currency_code =
        ui_->baseCurrencyEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.quote_currency_code =
        ui_->quoteCurrencyEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.price_source = ui_->priceSourceEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.gmm_initial_price =
        ui_->gmmInitialPriceEdit->text().trimmed().toDouble();
    fx_spot_generation_config_.ticks_per_hour = ui_->ticksPerHourEdit->value();
    fx_spot_generation_config_.process_type = ui_->processTypeEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.enabled = ui_->enabledCheck->isChecked();
    fx_spot_generation_config_.vintage_source =
        ui_->vintageSourceEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.vintage_date = ui_->vintageDateEdit->text().trimmed().toStdString();
    fx_spot_generation_config_.modified_by = username_;
}

void FxSpotGenerationConfigDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxSpotGenerationConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool FxSpotGenerationConfigDetailDialog::validateInput() {
    const QString base_currency_code_val = ui_->baseCurrencyEdit->text().trimmed();
    const QString quote_currency_code_val = ui_->quoteCurrencyEdit->text().trimmed();
    const QString price_source_val = ui_->priceSourceEdit->text().trimmed();
    const QString process_type_val = ui_->processTypeEdit->text().trimmed();
    const QString gmm_initial_price_val = ui_->gmmInitialPriceEdit->text().trimmed();
    const QString vintage_source_val = ui_->vintageSourceEdit->text().trimmed();
    const QString vintage_date_val = ui_->vintageDateEdit->text().trimmed();

    // price_source is a discriminator: "fixed" requires an initial price and
    // no vintage; "vintage" requires both vintage fields and no initial
    // price. Matches the DB check constraint.
    bool price_mode_valid = false;
    if (price_source_val == "fixed") {
        price_mode_valid = !gmm_initial_price_val.isEmpty() &&
                           gmm_initial_price_val.toDouble() > 0 && vintage_source_val.isEmpty() &&
                           vintage_date_val.isEmpty();
    } else if (price_source_val == "vintage") {
        price_mode_valid =
            (gmm_initial_price_val.isEmpty() || gmm_initial_price_val.toDouble() == 0) &&
            !vintage_source_val.isEmpty() && !vintage_date_val.isEmpty();
    }

    return true && !base_currency_code_val.isEmpty() && !quote_currency_code_val.isEmpty() &&
           !process_type_val.isEmpty() && price_mode_valid;
}

void FxSpotGenerationConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save FX spot generation config while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }

    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    fx_spot_generation_config_.change_reason_code = crSel->reason_code;
    fx_spot_generation_config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving FX spot generation config: "
                              << boost::uuids::to_string(fx_spot_generation_config_.id);

    QPointer<FxSpotGenerationConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, fx_spot_generation_config = fx_spot_generation_config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::save_fx_spot_generation_config_request request;
        request.data = fx_spot_generation_config;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "FX Spot Generation Config saved successfully";
            QString code = QString::fromStdString(
                boost::uuids::to_string(self->fx_spot_generation_config_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->fx_spot_generation_configSaved(code);
            self->notifySaveSuccess(tr("FX Spot Generation Config '%1' saved").arg(code));
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

void FxSpotGenerationConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete FX spot generation config while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(fx_spot_generation_config_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete FX Spot Generation Config",
        QString("Are you sure you want to delete FX spot generation config '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting FX spot generation config: "
                              << boost::uuids::to_string(fx_spot_generation_config_.id);

    QPointer<FxSpotGenerationConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task =
        [self, id_str = boost::uuids::to_string(fx_spot_generation_config_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        synthetic::messaging::delete_fx_spot_generation_config_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "FX Spot Generation Config deleted successfully";
            emit self->statusMessage(QString("FX Spot Generation Config '%1' deleted").arg(code));
            emit self->fx_spot_generation_configDeleted(code);
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
