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
#include "ores.qt/CreditInstrumentDetailDialog.hpp"

#include <QDate>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CreditInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CreditInstrumentDetailDialog::CreditInstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CreditInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CreditInstrumentDetailDialog::~CreditInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* CreditInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CreditInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CreditInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CreditInstrumentDetailDialog::setupUi() {
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

void CreditInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CreditInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CreditInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CreditInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->referenceEntityEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->spreadSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->recoveryRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->tenorEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->dayCountCodeCombo, &QComboBox::currentTextChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->paymentFrequencyCodeCombo, &QComboBox::currentTextChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->indexNameEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->indexSeriesSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->seniorityEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->restructuringEdit, &QLineEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &CreditInstrumentDetailDialog::onFieldChanged);
}

void CreditInstrumentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CreditInstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CreditInstrumentDetailDialog::setCreditInstrument(
    const trading::domain::credit_instrument& v) {
    instrument_ = v;
    updateUiFromCreditInstrument();
}

void CreditInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CreditInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->referenceEntityEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->spreadSpinBox->setReadOnly(readOnly);
    ui_->recoveryRateSpinBox->setReadOnly(readOnly);
    ui_->tenorEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->dayCountCodeCombo->setEnabled(!readOnly);
    ui_->paymentFrequencyCodeCombo->setEnabled(!readOnly);
    ui_->indexNameEdit->setReadOnly(readOnly);
    ui_->indexSeriesSpinBox->setReadOnly(readOnly);
    ui_->seniorityEdit->setReadOnly(readOnly);
    ui_->restructuringEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CreditInstrumentDetailDialog::updateUiFromCreditInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->referenceEntityEdit->setText(
        QString::fromStdString(instrument_.reference_entity));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->spreadSpinBox->setValue(instrument_.spread);
    ui_->recoveryRateSpinBox->setValue(instrument_.recovery_rate);
    ui_->tenorEdit->setText(
        QString::fromStdString(instrument_.tenor));
    ui_->startDateEdit->setText(
        QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->dayCountCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->paymentFrequencyCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.payment_frequency_code));
    ui_->indexNameEdit->setText(
        QString::fromStdString(instrument_.index_name));
    ui_->indexSeriesSpinBox->setValue(instrument_.index_series);
    ui_->seniorityEdit->setText(
        QString::fromStdString(instrument_.seniority));
    ui_->restructuringEdit->setText(
        QString::fromStdString(instrument_.restructuring));
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

void CreditInstrumentDetailDialog::updateCreditInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.reference_entity =
        ui_->referenceEntityEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.spread = ui_->spreadSpinBox->value();
    instrument_.recovery_rate = ui_->recoveryRateSpinBox->value();
    instrument_.tenor =
        ui_->tenorEdit->text().trimmed().toStdString();
    instrument_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.day_count_code =
        ui_->dayCountCodeCombo->currentText().trimmed().toStdString();
    instrument_.payment_frequency_code =
        ui_->paymentFrequencyCodeCombo->currentText().trimmed().toStdString();
    instrument_.index_name =
        ui_->indexNameEdit->text().trimmed().toStdString();
    instrument_.index_series = ui_->indexSeriesSpinBox->value();
    instrument_.seniority =
        ui_->seniorityEdit->text().trimmed().toStdString();
    instrument_.restructuring =
        ui_->restructuringEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CreditInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CreditInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool CreditInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->referenceEntityEdit->text().trimmed().isEmpty() &&
           !ui_->currencyEdit->text().trimmed().isEmpty() &&
           ui_->notionalSpinBox->value() > 0 &&
           !ui_->tenorEdit->text().trimmed().isEmpty() &&
           !ui_->dayCountCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->paymentFrequencyCodeCombo->currentText().trimmed().isEmpty() &&
           QDate::fromString(ui_->startDateEdit->text().trimmed(),
               "yyyy-MM-dd").isValid() &&
           QDate::fromString(ui_->maturityDateEdit->text().trimmed(),
               "yyyy-MM-dd").isValid();
}

void CreditInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save credit instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateCreditInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving credit instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<CreditInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_credit_instrument_request request;
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
            emit self->creditInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Credit instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CreditInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete credit instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Credit Instrument",
        QString("Are you sure you want to delete credit instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<CreditInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_credit_instrument_request request;
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
            emit self->creditInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Credit instrument '%1' deleted").arg(id));
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
