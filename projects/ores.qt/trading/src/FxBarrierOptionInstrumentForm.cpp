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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.qt/FxBarrierOptionInstrumentForm.hpp"

#include <QComboBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxBarrierOptionInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

FxBarrierOptionInstrumentForm::FxBarrierOptionInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::FxBarrierOptionInstrumentForm) {
    ui_->setupUi(this);
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    InstrumentFormUtils::populateSettlement(ui_->settlementCombo);
    InstrumentFormUtils::populateBarrierType(ui_->barrierTypeCombo);
    setupConnections();
}

FxBarrierOptionInstrumentForm::~FxBarrierOptionInstrumentForm() = default;

void FxBarrierOptionInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    auto markChangedStr = [this](const QString&) { onFieldChanged(); };
    auto markChangedDate = [this](const QDate&) { onFieldChanged(); };
    connect(ui_->boughtCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->soldCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->boughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->soldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->expiryDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->settlementCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->barrierTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->lowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->upperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void FxBarrierOptionInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxBarrierOptionInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->boughtCurrencyCombo, imageCache_,
                     FlagSource::Currency);
    setup_flag_combo(this, ui_->soldCurrencyCombo, imageCache_,
                     FlagSource::Currency);
}

void FxBarrierOptionInstrumentForm::populateCurrencies() {
    if (!clientManager_) return;

    QPointer<FxBarrierOptionInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self,
        [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        auto populate = [&codes](QComboBox* cb, const std::string& value) {
            cb->blockSignals(true);
            cb->clear();
            cb->addItem(QString());
            for (const auto& c : codes)
                cb->addItem(QString::fromStdString(c));
            InstrumentFormUtils::setComboValue(cb, value);
            cb->blockSignals(false);
        };

        populate(self->ui_->boughtCurrencyCombo, self->instrument_.bought_currency);
        populate(self->ui_->soldCurrencyCombo, self->instrument_.sold_currency);

        if (self->imageCache_) {
            apply_flag_icons(self->ui_->boughtCurrencyCombo,
                             self->imageCache_, FlagSource::Currency);
            apply_flag_icons(self->ui_->soldCurrencyCombo,
                             self->imageCache_, FlagSource::Currency);
        }
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() {
        return fetch_currency_codes(cm);
    }));
}

void FxBarrierOptionInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxBarrierOptionInstrumentForm::clear() {
    const std::string ttc = instrument_.trade_type_code;
    instrument_ = trading::domain::fx_barrier_option_instrument{};
    instrument_.trade_type_code = ttc;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
}

void FxBarrierOptionInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
}

void FxBarrierOptionInstrumentForm::setReadOnly(bool readOnly) {
    ui_->boughtCurrencyCombo->setEnabled(!readOnly);
    ui_->boughtAmountSpinBox->setReadOnly(readOnly);
    ui_->soldCurrencyCombo->setEnabled(!readOnly);
    ui_->soldAmountSpinBox->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->expiryDateEdit->setReadOnly(readOnly);
    ui_->settlementCombo->setEnabled(!readOnly);
    ui_->barrierTypeCombo->setEnabled(!readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxBarrierOptionInstrumentForm::isDirty() const { return dirty_; }
bool FxBarrierOptionInstrumentForm::isLoaded() const { return loaded_; }

void FxBarrierOptionInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxBarrierOptionInstrumentForm::writeUiToInstrument() {
    instrument_.bought_currency =
        InstrumentFormUtils::getComboValue(ui_->boughtCurrencyCombo);
    instrument_.bought_amount = ui_->boughtAmountSpinBox->value();
    instrument_.sold_currency =
        InstrumentFormUtils::getComboValue(ui_->soldCurrencyCombo);
    instrument_.sold_amount = ui_->soldAmountSpinBox->value();
    instrument_.option_type =
        InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    instrument_.expiry_date = ui_->expiryDateEdit->isoDate();
    instrument_.settlement =
        InstrumentFormUtils::getComboValue(ui_->settlementCombo);
    instrument_.barrier_type =
        InstrumentFormUtils::getComboValue(ui_->barrierTypeCombo);
    instrument_.lower_barrier = ui_->lowerBarrierSpinBox->value();
    {
        const double v = ui_->upperBarrierSpinBox->value();
        instrument_.upper_barrier = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.underlying_code =
        ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxBarrierOptionInstrumentForm::populate(
    const trading::domain::fx_barrier_option_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxBarrierOptionInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->boughtCurrencyCombo->blockSignals(b);
        ui_->boughtAmountSpinBox->blockSignals(b);
        ui_->soldCurrencyCombo->blockSignals(b);
        ui_->soldAmountSpinBox->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->expiryDateEdit->blockSignals(b);
        ui_->settlementCombo->blockSignals(b);
        ui_->barrierTypeCombo->blockSignals(b);
        ui_->lowerBarrierSpinBox->blockSignals(b);
        ui_->upperBarrierSpinBox->blockSignals(b);
        ui_->underlyingCodeEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    InstrumentFormUtils::setComboValue(ui_->boughtCurrencyCombo,
        instrument_.bought_currency);
    ui_->boughtAmountSpinBox->setValue(instrument_.bought_amount);
    InstrumentFormUtils::setComboValue(ui_->soldCurrencyCombo,
        instrument_.sold_currency);
    ui_->soldAmountSpinBox->setValue(instrument_.sold_amount);
    InstrumentFormUtils::setComboValue(ui_->optionTypeCombo, instrument_.option_type);
    ui_->expiryDateEdit->setIsoDate(instrument_.expiry_date);
    InstrumentFormUtils::setComboValue(ui_->settlementCombo, instrument_.settlement);
    InstrumentFormUtils::setComboValue(ui_->barrierTypeCombo, instrument_.barrier_type);
    ui_->lowerBarrierSpinBox->setValue(instrument_.lower_barrier);
    ui_->upperBarrierSpinBox->setValue(instrument_.upper_barrier.value_or(0.0));
    ui_->underlyingCodeEdit->setText(
        QString::fromStdString(instrument_.underlying_code));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    block(false);
}

void FxBarrierOptionInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxBarrierOptionInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void FxBarrierOptionInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<FxBarrierOptionInstrumentForm> self = this;
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
                << "FX barrier option save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX barrier option instrument saved";
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
        trading::messaging::save_fx_barrier_option_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
