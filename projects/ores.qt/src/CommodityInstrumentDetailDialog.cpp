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
#include "ores.qt/CommodityInstrumentDetailDialog.hpp"

#include <QMessageBox>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <QPlainTextEdit>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CommodityInstrumentDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CommodityInstrumentDetailDialog::CommodityInstrumentDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CommodityInstrumentDetailDialog),
      clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupConnections();
}

CommodityInstrumentDetailDialog::~CommodityInstrumentDetailDialog() {
    delete ui_;
}

QTabWidget* CommodityInstrumentDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CommodityInstrumentDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CommodityInstrumentDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

void CommodityInstrumentDetailDialog::setupUi() {
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

void CommodityInstrumentDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CommodityInstrumentDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CommodityInstrumentDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this,
            &CommodityInstrumentDetailDialog::onCloseClicked);

    connect(ui_->tradeTypeCodeCombo, &QComboBox::currentTextChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->commodityCodeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->quantitySpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->unitEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->fixedPriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->exerciseTypeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->strikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->barrierTypeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->lowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->upperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->averageTypeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->averagingStartDateEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->averagingEndDateEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->spreadCommodityCodeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->spreadAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->stripFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->varianceStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->accumulationAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->knockOutBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->dayCountCodeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->paymentFrequencyCodeEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->swaptionExpiryDateEdit, &QLineEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->basketJsonEdit, &QPlainTextEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this,
            &CommodityInstrumentDetailDialog::onFieldChanged);
}

void CommodityInstrumentDetailDialog::setClientManager(
    ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void CommodityInstrumentDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CommodityInstrumentDetailDialog::setCommodityInstrument(
    const trading::domain::commodity_instrument& v) {
    instrument_ = v;
    updateUiFromCommodityInstrument();
}

void CommodityInstrumentDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CommodityInstrumentDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->tradeTypeCodeCombo->setEnabled(!readOnly);
    ui_->commodityCodeEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->quantitySpinBox->setReadOnly(readOnly);
    ui_->unitEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->fixedPriceSpinBox->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->exerciseTypeEdit->setReadOnly(readOnly);
    ui_->strikePriceSpinBox->setReadOnly(readOnly);
    ui_->barrierTypeEdit->setReadOnly(readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->averageTypeEdit->setReadOnly(readOnly);
    ui_->averagingStartDateEdit->setReadOnly(readOnly);
    ui_->averagingEndDateEdit->setReadOnly(readOnly);
    ui_->spreadCommodityCodeEdit->setReadOnly(readOnly);
    ui_->spreadAmountSpinBox->setReadOnly(readOnly);
    ui_->stripFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->varianceStrikeSpinBox->setReadOnly(readOnly);
    ui_->accumulationAmountSpinBox->setReadOnly(readOnly);
    ui_->knockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->dayCountCodeEdit->setReadOnly(readOnly);
    ui_->paymentFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->swaptionExpiryDateEdit->setReadOnly(readOnly);
    ui_->basketJsonEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void CommodityInstrumentDetailDialog::updateUiFromCommodityInstrument() {
    ui_->tradeTypeCodeCombo->setCurrentText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->commodityCodeEdit->setText(
        QString::fromStdString(instrument_.commodity_code));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->quantitySpinBox->setValue(instrument_.quantity);
    ui_->unitEdit->setText(
        QString::fromStdString(instrument_.unit));
    ui_->startDateEdit->setText(
        QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->fixedPriceSpinBox->setValue(instrument_.fixed_price.value_or(0.0));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->exerciseTypeEdit->setText(
        QString::fromStdString(instrument_.exercise_type));
    ui_->strikePriceSpinBox->setValue(instrument_.strike_price.value_or(0.0));
    ui_->barrierTypeEdit->setText(
        QString::fromStdString(instrument_.barrier_type));
    ui_->lowerBarrierSpinBox->setValue(instrument_.lower_barrier.value_or(0.0));
    ui_->upperBarrierSpinBox->setValue(instrument_.upper_barrier.value_or(0.0));
    ui_->averageTypeEdit->setText(
        QString::fromStdString(instrument_.average_type));
    ui_->averagingStartDateEdit->setText(
        QString::fromStdString(instrument_.averaging_start_date));
    ui_->averagingEndDateEdit->setText(
        QString::fromStdString(instrument_.averaging_end_date));
    ui_->spreadCommodityCodeEdit->setText(
        QString::fromStdString(instrument_.spread_commodity_code));
    ui_->spreadAmountSpinBox->setValue(instrument_.spread_amount.value_or(0.0));
    ui_->stripFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.strip_frequency_code));
    ui_->varianceStrikeSpinBox->setValue(instrument_.variance_strike.value_or(0.0));
    ui_->accumulationAmountSpinBox->setValue(instrument_.accumulation_amount.value_or(0.0));
    ui_->knockOutBarrierSpinBox->setValue(instrument_.knock_out_barrier.value_or(0.0));
    ui_->dayCountCodeEdit->setText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->paymentFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.payment_frequency_code));
    ui_->swaptionExpiryDateEdit->setText(
        QString::fromStdString(instrument_.swaption_expiry_date));
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

void CommodityInstrumentDetailDialog::updateCommodityInstrumentFromUi() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeCombo->currentText().trimmed().toStdString();
    instrument_.commodity_code =
        ui_->commodityCodeEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.quantity = ui_->quantitySpinBox->value();
    instrument_.unit =
        ui_->unitEdit->text().trimmed().toStdString();
    instrument_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    {
        const double v = ui_->fixedPriceSpinBox->value();
        instrument_.fixed_price = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.exercise_type =
        ui_->exerciseTypeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->strikePriceSpinBox->value();
        instrument_.strike_price = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.barrier_type =
        ui_->barrierTypeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->lowerBarrierSpinBox->value();
        instrument_.lower_barrier = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    {
        const double v = ui_->upperBarrierSpinBox->value();
        instrument_.upper_barrier = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.average_type =
        ui_->averageTypeEdit->text().trimmed().toStdString();
    instrument_.averaging_start_date =
        ui_->averagingStartDateEdit->text().trimmed().toStdString();
    instrument_.averaging_end_date =
        ui_->averagingEndDateEdit->text().trimmed().toStdString();
    instrument_.spread_commodity_code =
        ui_->spreadCommodityCodeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->spreadAmountSpinBox->value();
        instrument_.spread_amount = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.strip_frequency_code =
        ui_->stripFrequencyCodeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->varianceStrikeSpinBox->value();
        instrument_.variance_strike = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    {
        const double v = ui_->accumulationAmountSpinBox->value();
        instrument_.accumulation_amount = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    {
        const double v = ui_->knockOutBarrierSpinBox->value();
        instrument_.knock_out_barrier = v != 0.0 ? std::optional(v) : std::nullopt;
    }
    instrument_.day_count_code =
        ui_->dayCountCodeEdit->text().trimmed().toStdString();
    instrument_.payment_frequency_code =
        ui_->paymentFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.swaption_expiry_date =
        ui_->swaptionExpiryDateEdit->text().trimmed().toStdString();
    instrument_.basket_json =
        ui_->basketJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CommodityInstrumentDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CommodityInstrumentDetailDialog::updateSaveButtonState() {
    ui_->saveButton->setEnabled(hasChanges_ && validateInput() && !readOnly_);
}

bool CommodityInstrumentDetailDialog::validateInput() {
    return !ui_->tradeTypeCodeCombo->currentText().trimmed().isEmpty() &&
           !ui_->commodityCodeEdit->text().trimmed().isEmpty() &&
           !ui_->currencyEdit->text().trimmed().isEmpty() &&
           !ui_->unitEdit->text().trimmed().isEmpty() &&
           ui_->quantitySpinBox->value() > 0;
}

void CommodityInstrumentDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save commodity instrument while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateCommodityInstrumentFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving commodity instrument: "
                              << boost::uuids::to_string(instrument_.id);

    QPointer<CommodityInstrumentDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, instrument = instrument_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::save_commodity_instrument_request request;
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
            emit self->commodityInstrumentSaved(id);
            self->notifySaveSuccess(
                tr("Commodity instrument '%1' saved").arg(id));
        } else {
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CommodityInstrumentDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete commodity instrument while disconnected from server.");
        return;
    }

    QString id = QString::fromStdString(
        boost::uuids::to_string(instrument_.id));
    auto reply = MessageBoxHelper::question(this, "Delete Commodity Instrument",
        QString("Are you sure you want to delete commodity instrument '%1'?").arg(id),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<CommodityInstrumentDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(instrument_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        trading::messaging::delete_commodity_instrument_request request;
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
            emit self->commodityInstrumentDeleted(id);
            emit self->statusMessage(
                tr("Commodity instrument '%1' deleted").arg(id));
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
