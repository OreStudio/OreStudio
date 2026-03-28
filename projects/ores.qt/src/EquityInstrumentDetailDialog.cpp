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
#include "ores.qt/EquityInstrumentDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_EquityInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

EquityInstrumentDetailDialog::EquityInstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::EquityInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

EquityInstrumentDetailDialog::~EquityInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* EquityInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* EquityInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* EquityInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void EquityInstrumentDetailDialog::setupUi() {
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

void EquityInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &EquityInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &EquityInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &EquityInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->quantitySpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->exerciseTypeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->strikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->barrierTypeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->lowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->upperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->averageTypeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->averagingStartDateEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->varianceStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->cliquetFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->accumulationAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->knockOutBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->dayCountCodeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->paymentFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->returnTypeEdit, &QLineEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->basketJsonEdit, &QPlainTextEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &EquityInstrumentDetailDialog::onFieldChanged);
}

void EquityInstrumentDetailDialog::setClientManager(
    ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void EquityInstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void EquityInstrumentDetailDialog::setEquityInstrument(
    const trading::domain::equity_instrument& v) {
    instrument_ = v;
    updateUiFromEquityInstrument();
}

void EquityInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void EquityInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->quantitySpinBox->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->exerciseTypeEdit->setReadOnly(readOnly);
    ui_->strikePriceSpinBox->setReadOnly(readOnly);
    ui_->barrierTypeEdit->setReadOnly(readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->averageTypeEdit->setReadOnly(readOnly);
    ui_->averagingStartDateEdit->setReadOnly(readOnly);
    ui_->varianceStrikeSpinBox->setReadOnly(readOnly);
    ui_->cliquetFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->accumulationAmountSpinBox->setReadOnly(readOnly);
    ui_->knockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->dayCountCodeEdit->setReadOnly(readOnly);
    ui_->paymentFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->returnTypeEdit->setReadOnly(readOnly);
    ui_->basketJsonEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void EquityInstrumentDetailDialog::updateUiFromEquityInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->underlyingCodeEdit->setText(
        QString::fromStdString(instrument_.underlying_code));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->quantitySpinBox->setValue(instrument_.quantity);
    ui_->startDateEdit->setText(
        QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->exerciseTypeEdit->setText(
        QString::fromStdString(instrument_.exercise_type));
    ui_->strikePriceSpinBox->setValue(instrument_.strike_price);
    ui_->barrierTypeEdit->setText(
        QString::fromStdString(instrument_.barrier_type));
    ui_->lowerBarrierSpinBox->setValue(instrument_.lower_barrier);
    ui_->upperBarrierSpinBox->setValue(instrument_.upper_barrier);
    ui_->averageTypeEdit->setText(
        QString::fromStdString(instrument_.average_type));
    ui_->averagingStartDateEdit->setText(
        QString::fromStdString(instrument_.averaging_start_date));
    ui_->varianceStrikeSpinBox->setValue(instrument_.variance_strike);
    ui_->cliquetFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.cliquet_frequency_code));
    ui_->accumulationAmountSpinBox->setValue(instrument_.accumulation_amount);
    ui_->knockOutBarrierSpinBox->setValue(instrument_.knock_out_barrier);
    ui_->dayCountCodeEdit->setText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->paymentFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.payment_frequency_code));
    ui_->returnTypeEdit->setText(
        QString::fromStdString(instrument_.return_type));
    ui_->basketJsonEdit->setPlainText(
        QString::fromStdString(instrument_.basket_json));
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

void EquityInstrumentDetailDialog::updateEquityInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.underlying_code =
        ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.quantity = ui_->quantitySpinBox->value();
    instrument_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.exercise_type =
        ui_->exerciseTypeEdit->text().trimmed().toStdString();
    instrument_.strike_price = ui_->strikePriceSpinBox->value();
    instrument_.barrier_type =
        ui_->barrierTypeEdit->text().trimmed().toStdString();
    instrument_.lower_barrier = ui_->lowerBarrierSpinBox->value();
    instrument_.upper_barrier = ui_->upperBarrierSpinBox->value();
    instrument_.average_type =
        ui_->averageTypeEdit->text().trimmed().toStdString();
    instrument_.averaging_start_date =
        ui_->averagingStartDateEdit->text().trimmed().toStdString();
    instrument_.variance_strike = ui_->varianceStrikeSpinBox->value();
    instrument_.cliquet_frequency_code =
        ui_->cliquetFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.accumulation_amount = ui_->accumulationAmountSpinBox->value();
    instrument_.knock_out_barrier = ui_->knockOutBarrierSpinBox->value();
    instrument_.day_count_code =
        ui_->dayCountCodeEdit->text().trimmed().toStdString();
    instrument_.payment_frequency_code =
        ui_->paymentFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.return_type =
        ui_->returnTypeEdit->text().trimmed().toStdString();
    instrument_.basket_json =
        ui_->basketJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void EquityInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void EquityInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool EquityInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->underlyingCodeEdit->text().trimmed().isEmpty() &&
           !ui_->currencyEdit->text().trimmed().isEmpty() &&
           ui_->notionalSpinBox->value() > 0;
}

void EquityInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save equity instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateEquityInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving equity instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<EquityInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_equity_instrument_request request;
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
            emit self->equityInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Equity instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EquityInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete equity instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Equity Instrument",
        QString("Are you sure you want to delete equity instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<EquityInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_equity_instrument_request request;
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
            emit self->equityInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Equity instrument '%1' deleted").arg(id));
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
