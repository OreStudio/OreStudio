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
#include "ores.qt/InstrumentDetailDialog.hpp"

#include <QDate>
#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_InstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

InstrumentDetailDialog::InstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::InstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

InstrumentDetailDialog::~InstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* InstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* InstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* InstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void InstrumentDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void InstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &InstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &InstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &InstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->fraFixingDateEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->fraSettlementDateEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->lockoutDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->callableDatesJsonEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->rpaCounterpartyEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->inflationIndexCodeEdit, &QLineEdit::textChanged, this,
            &InstrumentDetailDialog::onFieldChanged);
    connect(ui_->baseCpiSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &InstrumentDetailDialog::onFieldChanged);
}

void InstrumentDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void InstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void InstrumentDetailDialog::setInstrument(
    const trading::domain::instrument& v) {
    instrument_ = v;
    updateUiFromInstrument();
}

void InstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void InstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->fraFixingDateEdit->setReadOnly(readOnly);
    ui_->fraSettlementDateEdit->setReadOnly(readOnly);
    ui_->lockoutDaysSpinBox->setReadOnly(readOnly);
    ui_->callableDatesJsonEdit->setReadOnly(readOnly);
    ui_->rpaCounterpartyEdit->setReadOnly(readOnly);
    ui_->inflationIndexCodeEdit->setReadOnly(readOnly);
    ui_->baseCpiSpinBox->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void InstrumentDetailDialog::updateUiFromInstrument() {
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->currencyEdit->setText(QString::fromStdString(instrument_.currency));
    ui_->startDateEdit->setText(QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->fraFixingDateEdit->setText(
        QString::fromStdString(instrument_.fra_fixing_date));
    ui_->fraSettlementDateEdit->setText(
        QString::fromStdString(instrument_.fra_settlement_date));
    ui_->lockoutDaysSpinBox->setValue(
        instrument_.lockout_days.value_or(0));
    ui_->callableDatesJsonEdit->setText(
        QString::fromStdString(instrument_.callable_dates_json));
    ui_->rpaCounterpartyEdit->setText(
        QString::fromStdString(instrument_.rpa_counterparty));
    ui_->inflationIndexCodeEdit->setText(
        QString::fromStdString(instrument_.inflation_index_code));
    ui_->baseCpiSpinBox->setValue(
        instrument_.base_cpi.value_or(0.0));

    populateProvenance(instrument_.version,
                       instrument_.modified_by,
                       instrument_.performed_by,
                       instrument_.recorded_at,
                       instrument_.change_reason_code,
                       instrument_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void InstrumentDetailDialog::updateInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.currency = ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.start_date = ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.fra_fixing_date =
        ui_->fraFixingDateEdit->text().trimmed().toStdString();
    instrument_.fra_settlement_date =
        ui_->fraSettlementDateEdit->text().trimmed().toStdString();
    {
        int v = ui_->lockoutDaysSpinBox->value();
        instrument_.lockout_days = v != 0 ? std::optional(v) : std::nullopt;
    }
    instrument_.callable_dates_json =
        ui_->callableDatesJsonEdit->text().trimmed().toStdString();
    instrument_.rpa_counterparty =
        ui_->rpaCounterpartyEdit->text().trimmed().toStdString();
    instrument_.inflation_index_code =
        ui_->inflationIndexCodeEdit->text().trimmed().toStdString();
    {
        double v = ui_->baseCpiSpinBox->value();
        instrument_.base_cpi = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void InstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void InstrumentDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool InstrumentDetailDialog::validateInput() {
    const QString trade_type = ui_->tradeTypeCodeEdit->text().trimmed();
    return !trade_type.isEmpty();
}

void InstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<InstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_instrument_request request;
        request.data = instrument;
        request.legs = {};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

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
            BOOST_LOG_SEV(lg(), info) << "Instrument saved successfully";
            QString id = QString::fromStdString(
                boost::uuids::to_string(self->instrument_.id));
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->instrumentSaved(id);
            self->notifySaveSuccess(tr("Instrument '%1' saved").arg(id));
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

void InstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Instrument",
        QString("Are you sure you want to delete instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<InstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_instrument_request request;
        request.ids = {id_str};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

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
            BOOST_LOG_SEV(lg(), info) << "Instrument deleted successfully";
            emit self->statusMessage(
                QString("Instrument '%1' deleted").arg(id));
            emit self->instrumentDeleted(id);
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
