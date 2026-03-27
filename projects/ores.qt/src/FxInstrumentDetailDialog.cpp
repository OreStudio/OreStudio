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
#include "ores.qt/FxInstrumentDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

FxInstrumentDetailDialog::FxInstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::FxInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

FxInstrumentDetailDialog::~FxInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* FxInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* FxInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* FxInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void FxInstrumentDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);
    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(
            Icon::Delete, IconUtils::DefaultIconColor));
    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(
            Icon::Dismiss, IconUtils::DefaultIconColor));
}

void FxInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &FxInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &FxInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &FxInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->boughtCurrencyEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->soldCurrencyEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->valueDateEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->settlementEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->expiryDateEdit, &QLineEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->boughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->soldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &FxInstrumentDetailDialog::onFieldChanged);
    connect(ui_->strikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &FxInstrumentDetailDialog::onFieldChanged);
}

void FxInstrumentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void FxInstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void FxInstrumentDetailDialog::setFxInstrument(
    const trading::domain::fx_instrument& v) {
    instrument_ = v;
    updateUiFromFxInstrument();
}

void FxInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void FxInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->boughtCurrencyEdit->setReadOnly(readOnly);
    ui_->boughtAmountSpinBox->setReadOnly(readOnly);
    ui_->soldCurrencyEdit->setReadOnly(readOnly);
    ui_->soldAmountSpinBox->setReadOnly(readOnly);
    ui_->valueDateEdit->setReadOnly(readOnly);
    ui_->settlementEdit->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->strikePriceSpinBox->setReadOnly(readOnly);
    ui_->expiryDateEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void FxInstrumentDetailDialog::updateUiFromFxInstrument() {
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->boughtCurrencyEdit->setText(
        QString::fromStdString(instrument_.bought_currency));
    ui_->boughtAmountSpinBox->setValue(instrument_.bought_amount);
    ui_->soldCurrencyEdit->setText(
        QString::fromStdString(instrument_.sold_currency));
    ui_->soldAmountSpinBox->setValue(instrument_.sold_amount);
    ui_->valueDateEdit->setText(
        QString::fromStdString(instrument_.value_date));
    ui_->settlementEdit->setText(
        QString::fromStdString(instrument_.settlement));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->strikePriceSpinBox->setValue(instrument_.strike_price);
    ui_->expiryDateEdit->setText(
        QString::fromStdString(instrument_.expiry_date));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));

    populateProvenance(instrument_.version,
                       instrument_.modified_by,
                       instrument_.performed_by,
                       instrument_.recorded_at,
                       instrument_.change_reason_code,
                       instrument_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void FxInstrumentDetailDialog::updateFxInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.bought_currency =
        ui_->boughtCurrencyEdit->text().trimmed().toStdString();
    instrument_.bought_amount = ui_->boughtAmountSpinBox->value();
    instrument_.sold_currency =
        ui_->soldCurrencyEdit->text().trimmed().toStdString();
    instrument_.sold_amount = ui_->soldAmountSpinBox->value();
    instrument_.value_date =
        ui_->valueDateEdit->text().trimmed().toStdString();
    instrument_.settlement =
        ui_->settlementEdit->text().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.strike_price = ui_->strikePriceSpinBox->value();
    instrument_.expiry_date =
        ui_->expiryDateEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void FxInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool FxInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeEdit->text().trimmed().isEmpty() &&
           !ui_->boughtCurrencyEdit->text().trimmed().isEmpty() &&
           !ui_->soldCurrencyEdit->text().trimmed().isEmpty() &&
           !ui_->valueDateEdit->text().trimmed().isEmpty();
}

void FxInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save FX instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateFxInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving FX instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<FxInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_fx_instrument_request request;
        request.data = instrument;
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

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
            QString id = QString::fromStdString(
                boost::uuids::to_string(self->instrument_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->fxInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("FX instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void FxInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete FX instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete FX Instrument",
        QString("Are you sure you want to delete FX instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<FxInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_fx_instrument_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(
                std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, id, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            emit self->fxInstrumentDeleted(id);
            emit self->statusMessage(
                tr("FX instrument '%1' deleted").arg(id));
            self->requestClose();
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
