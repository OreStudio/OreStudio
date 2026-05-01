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
#include "ores.qt/EquityInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_EquityInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

EquityInstrumentForm::EquityInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::EquityInstrumentForm) {
    ui_->setupUi(this);
    // Options and extensions tabs hidden until setTradeType() reveals them.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->optionsTab), false);
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), false);
    setupConnections();
}

EquityInstrumentForm::~EquityInstrumentForm() = default;

void EquityInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->exerciseTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->barrierTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->averageTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->averagingStartDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->cliquetFrequencyCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->dayCountCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->paymentFrequencyCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->returnTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->basketJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->quantitySpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->strikePriceSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->lowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->upperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->varianceStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->accumulationAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->knockOutBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void EquityInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void EquityInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void EquityInstrumentForm::clear() {
    instrument_ = trading::domain::equity_option_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void EquityInstrumentForm::setTradeType(const QString& code,
    bool has_options, bool has_extension) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->optionsTab), has_options);
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void EquityInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->quantitySpinBox->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
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
}

bool EquityInstrumentForm::isDirty() const { return dirty_; }
bool EquityInstrumentForm::isLoaded() const { return loaded_; }

void EquityInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void EquityInstrumentForm::writeUiToInstrument() {
    // Scoped to equity_option_instrument. Widgets outside this struct's
    // fields (barrier, averaging, variance, accumulation, return-type,
    // day-count, basket) are left in place in the .ui for future variant
    // coverage but are not read back here.
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.underlying_name =
        ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.expiry_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.exercise_type =
        ui_->exerciseTypeEdit->text().trimmed().toStdString();
    instrument_.strike = ui_->strikePriceSpinBox->value();
    instrument_.cliquet_frequency =
        ui_->cliquetFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void EquityInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* ex =
        std::get_if<trading::messaging::equity_export_result>(&instrument);
    if (!ex) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-equity instrument pushed to EquityInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for equity form"));
        return;
    }
    const auto* opt =
        std::get_if<trading::domain::equity_option_instrument>(&ex->instrument);
    if (!opt) {
        emit loadFailed(QStringLiteral(
            "Non-option equity type not yet supported in this dialog"));
        return;
    }

    instrument_ = *opt;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void EquityInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->underlyingCodeEdit->blockSignals(b);
        ui_->currencyEdit->blockSignals(b);
        ui_->notionalSpinBox->blockSignals(b);
        ui_->quantitySpinBox->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->optionTypeEdit->blockSignals(b);
        ui_->exerciseTypeEdit->blockSignals(b);
        ui_->strikePriceSpinBox->blockSignals(b);
        ui_->barrierTypeEdit->blockSignals(b);
        ui_->lowerBarrierSpinBox->blockSignals(b);
        ui_->upperBarrierSpinBox->blockSignals(b);
        ui_->averageTypeEdit->blockSignals(b);
        ui_->averagingStartDateEdit->blockSignals(b);
        ui_->varianceStrikeSpinBox->blockSignals(b);
        ui_->cliquetFrequencyCodeEdit->blockSignals(b);
        ui_->accumulationAmountSpinBox->blockSignals(b);
        ui_->knockOutBarrierSpinBox->blockSignals(b);
        ui_->dayCountCodeEdit->blockSignals(b);
        ui_->paymentFrequencyCodeEdit->blockSignals(b);
        ui_->returnTypeEdit->blockSignals(b);
        ui_->basketJsonEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->underlyingCodeEdit->setText(
        QString::fromStdString(instrument_.underlying_name));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->quantitySpinBox->setValue(0.0);
    ui_->startDateEdit->clear();
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.expiry_date));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->exerciseTypeEdit->setText(
        QString::fromStdString(instrument_.exercise_type));
    ui_->strikePriceSpinBox->setValue(instrument_.strike);
    ui_->barrierTypeEdit->clear();
    ui_->lowerBarrierSpinBox->setValue(0.0);
    ui_->upperBarrierSpinBox->setValue(0.0);
    ui_->averageTypeEdit->clear();
    ui_->averagingStartDateEdit->clear();
    ui_->varianceStrikeSpinBox->setValue(0.0);
    ui_->cliquetFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.cliquet_frequency));
    ui_->accumulationAmountSpinBox->setValue(0.0);
    ui_->knockOutBarrierSpinBox->setValue(0.0);
    ui_->dayCountCodeEdit->clear();
    ui_->paymentFrequencyCodeEdit->clear();
    ui_->returnTypeEdit->clear();
    ui_->basketJsonEdit->clear();
    block(false);
}

void EquityInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void EquityInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void EquityInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<EquityInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self,
        [self, watcher,
         on_success = std::move(on_success),
         on_failure = std::move(on_failure)]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), error)
                << "Equity instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Equity instrument saved";
        self->dirty_ = false;
        self->emitProvenance();
        on_success(boost::uuids::to_string(self->instrument_.instrument_id));
    });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_equity_option_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
