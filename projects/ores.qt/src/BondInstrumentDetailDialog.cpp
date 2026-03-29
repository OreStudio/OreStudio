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
#include "ores.qt/BondInstrumentDetailDialog.hpp"

#include <QDate>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_BondInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BondInstrumentDetailDialog::BondInstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::BondInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

BondInstrumentDetailDialog::~BondInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* BondInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* BondInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* BondInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void BondInstrumentDetailDialog::setupUi() {
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

void BondInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &BondInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &BondInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &BondInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->issuerEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->faceValueSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->couponRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->couponFrequencyCodeCombo, &QComboBox::currentTextChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->dayCountCodeCombo, &QComboBox::currentTextChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->issueDateEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->settlementDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->callDateEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->conversionRatioSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->futureExpiryDateEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionExpiryDateEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->trsReturnTypeCombo, &QComboBox::currentTextChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->trsFundingLegCodeEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
    connect(ui_->ascotOptionTypeEdit, &QLineEdit::textChanged, this,
            &BondInstrumentDetailDialog::onFieldChanged);
}

void BondInstrumentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void BondInstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void BondInstrumentDetailDialog::setBondInstrument(
    const trading::domain::bond_instrument& v) {
    instrument_ = v;
    updateUiFromBondInstrument();
}

void BondInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void BondInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->issuerEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->faceValueSpinBox->setReadOnly(readOnly);
    ui_->couponRateSpinBox->setReadOnly(readOnly);
    ui_->couponFrequencyCodeCombo->setEnabled(!readOnly);
    ui_->dayCountCodeCombo->setEnabled(!readOnly);
    ui_->issueDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->settlementDaysSpinBox->setReadOnly(readOnly);
    ui_->callDateEdit->setReadOnly(readOnly);
    ui_->conversionRatioSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->futureExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->optionExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionStrikeSpinBox->setReadOnly(readOnly);
    ui_->trsReturnTypeCombo->setEnabled(!readOnly);
    ui_->trsFundingLegCodeEdit->setReadOnly(readOnly);
    ui_->ascotOptionTypeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void BondInstrumentDetailDialog::updateUiFromBondInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->issuerEdit->setText(
        QString::fromStdString(instrument_.issuer));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->faceValueSpinBox->setValue(instrument_.face_value);
    ui_->couponRateSpinBox->setValue(instrument_.coupon_rate);
    ui_->couponFrequencyCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.coupon_frequency_code));
    ui_->dayCountCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->issueDateEdit->setText(
        QString::fromStdString(instrument_.issue_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->settlementDaysSpinBox->setValue(instrument_.settlement_days);
    ui_->callDateEdit->setText(
        QString::fromStdString(instrument_.call_date));
    ui_->conversionRatioSpinBox->setValue(instrument_.conversion_ratio);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->futureExpiryDateEdit->setText(
        QString::fromStdString(instrument_.future_expiry_date));
    ui_->optionTypeCombo->setCurrentText(
        QString::fromStdString(instrument_.option_type));
    ui_->optionExpiryDateEdit->setText(
        QString::fromStdString(instrument_.option_expiry_date));
    ui_->optionStrikeSpinBox->setValue(
        instrument_.option_strike.value_or(0.0));
    ui_->trsReturnTypeCombo->setCurrentText(
        QString::fromStdString(instrument_.trs_return_type));
    ui_->trsFundingLegCodeEdit->setText(
        QString::fromStdString(instrument_.trs_funding_leg_code));
    ui_->ascotOptionTypeEdit->setText(
        QString::fromStdString(instrument_.ascot_option_type));

    populateProvenance(instrument_.version,
                       instrument_.modified_by,
                       instrument_.performed_by,
                       instrument_.recorded_at,
                       instrument_.change_reason_code,
                       instrument_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void BondInstrumentDetailDialog::updateBondInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.issuer =
        ui_->issuerEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.face_value = ui_->faceValueSpinBox->value();
    instrument_.coupon_rate = ui_->couponRateSpinBox->value();
    instrument_.coupon_frequency_code =
        ui_->couponFrequencyCodeCombo->currentText().trimmed().toStdString();
    instrument_.day_count_code =
        ui_->dayCountCodeCombo->currentText().trimmed().toStdString();
    instrument_.issue_date =
        ui_->issueDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.settlement_days = ui_->settlementDaysSpinBox->value();
    instrument_.call_date =
        ui_->callDateEdit->text().trimmed().toStdString();
    instrument_.conversion_ratio = ui_->conversionRatioSpinBox->value();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.future_expiry_date =
        ui_->futureExpiryDateEdit->text().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeCombo->currentText().trimmed().toStdString();
    instrument_.option_expiry_date =
        ui_->optionExpiryDateEdit->text().trimmed().toStdString();
    instrument_.option_strike = nulloptIfZero(ui_->optionStrikeSpinBox->value());
    instrument_.trs_return_type =
        ui_->trsReturnTypeCombo->currentText().trimmed().toStdString();
    instrument_.trs_funding_leg_code =
        ui_->trsFundingLegCodeEdit->text().trimmed().toStdString();
    instrument_.ascot_option_type =
        ui_->ascotOptionTypeEdit->text().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void BondInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void BondInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool BondInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->issuerEdit->text().trimmed().isEmpty() &&
           !ui_->currencyEdit->text().trimmed().isEmpty() &&
           ui_->faceValueSpinBox->value() > 0 &&
           !ui_->couponFrequencyCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->dayCountCodeCombo->currentText().trimmed().isEmpty() &&
           QDate::fromString(ui_->issueDateEdit->text().trimmed(),
               "yyyy-MM-dd").isValid() &&
           QDate::fromString(ui_->maturityDateEdit->text().trimmed(),
               "yyyy-MM-dd").isValid();
}

void BondInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save bond instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateBondInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving bond instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<BondInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_bond_instrument_request request;
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
            emit self->bondInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Bond instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void BondInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete bond instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Bond Instrument",
        QString("Are you sure you want to delete bond instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<BondInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_bond_instrument_request request;
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
            emit self->bondInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Bond instrument '%1' deleted").arg(id));
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
