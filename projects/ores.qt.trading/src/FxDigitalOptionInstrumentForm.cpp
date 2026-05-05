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
#include "ores.qt/FxDigitalOptionInstrumentForm.hpp"

#include <QComboBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxDigitalOptionInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

FxDigitalOptionInstrumentForm::FxDigitalOptionInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::FxDigitalOptionInstrumentForm) {
    ui_->setupUi(this);
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    InstrumentFormUtils::populateLongShort(ui_->longShortCombo);
    InstrumentFormUtils::populateBarrierType(ui_->barrierTypeCombo);
    setupConnections();
}

FxDigitalOptionInstrumentForm::~FxDigitalOptionInstrumentForm() = default;

void FxDigitalOptionInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    auto markChangedStr = [this](const QString&) { onFieldChanged(); };
    auto markChangedDate = [this](const QDate&) { onFieldChanged(); };
    connect(ui_->foreignCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->domesticCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->payoffCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->payoffAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->expiryDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->longShortCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->strikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->barrierTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->lowerBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->upperBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void FxDigitalOptionInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxDigitalOptionInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->foreignCurrencyCombo, imageCache_,
                     FlagSource::Currency);
    setup_flag_combo(this, ui_->domesticCurrencyCombo, imageCache_,
                     FlagSource::Currency);
    setup_flag_combo(this, ui_->payoffCurrencyCombo, imageCache_,
                     FlagSource::Currency);
}

void FxDigitalOptionInstrumentForm::populateCurrencies() {
    if (!clientManager_) return;

    QPointer<FxDigitalOptionInstrumentForm> self = this;
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

        populate(self->ui_->foreignCurrencyCombo, self->instrument_.foreign_currency);
        populate(self->ui_->domesticCurrencyCombo, self->instrument_.domestic_currency);
        populate(self->ui_->payoffCurrencyCombo, self->instrument_.payoff_currency);

        if (self->imageCache_) {
            apply_flag_icons(self->ui_->foreignCurrencyCombo,
                             self->imageCache_, FlagSource::Currency);
            apply_flag_icons(self->ui_->domesticCurrencyCombo,
                             self->imageCache_, FlagSource::Currency);
            apply_flag_icons(self->ui_->payoffCurrencyCombo,
                             self->imageCache_, FlagSource::Currency);
        }
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() {
        return fetch_currency_codes(cm);
    }));
}

void FxDigitalOptionInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxDigitalOptionInstrumentForm::clear() {
    const std::string ttc = instrument_.trade_type_code;
    instrument_ = trading::domain::fx_digital_option_instrument{};
    instrument_.trade_type_code = ttc;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
}

void FxDigitalOptionInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
}

void FxDigitalOptionInstrumentForm::setReadOnly(bool readOnly) {
    ui_->foreignCurrencyCombo->setEnabled(!readOnly);
    ui_->domesticCurrencyCombo->setEnabled(!readOnly);
    ui_->payoffCurrencyCombo->setEnabled(!readOnly);
    ui_->payoffAmountSpinBox->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->expiryDateEdit->setReadOnly(readOnly);
    ui_->longShortCombo->setEnabled(!readOnly);
    ui_->strikeSpinBox->setReadOnly(readOnly);
    ui_->barrierTypeCombo->setEnabled(!readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxDigitalOptionInstrumentForm::isDirty() const { return dirty_; }
bool FxDigitalOptionInstrumentForm::isLoaded() const { return loaded_; }

void FxDigitalOptionInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxDigitalOptionInstrumentForm::writeUiToInstrument() {
    instrument_.foreign_currency =
        InstrumentFormUtils::getComboValue(ui_->foreignCurrencyCombo);
    instrument_.domestic_currency =
        InstrumentFormUtils::getComboValue(ui_->domesticCurrencyCombo);
    instrument_.payoff_currency =
        InstrumentFormUtils::getComboValue(ui_->payoffCurrencyCombo);
    instrument_.payoff_amount = ui_->payoffAmountSpinBox->value();
    instrument_.option_type =
        InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    instrument_.expiry_date = ui_->expiryDateEdit->isoDate();
    instrument_.long_short =
        InstrumentFormUtils::getComboValue(ui_->longShortCombo);
    {
        const double s = ui_->strikeSpinBox->value();
        instrument_.strike = (s > 0.0) ? std::optional<double>(s) : std::nullopt;
    }
    instrument_.barrier_type =
        InstrumentFormUtils::getComboValue(ui_->barrierTypeCombo);
    {
        const double v = ui_->lowerBarrierSpinBox->value();
        instrument_.lower_barrier = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->upperBarrierSpinBox->value();
        instrument_.upper_barrier = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxDigitalOptionInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* ex =
        std::get_if<trading::messaging::fx_export_result>(&instrument);
    if (!ex) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-FX instrument pushed to FxDigitalOptionInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for FX digital option form"));
        return;
    }
    const auto* di =
        std::get_if<trading::domain::fx_digital_option_instrument>(&ex->instrument);
    if (!di) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-digital-option FX type pushed to FxDigitalOptionInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected FX sub-type for digital option form"));
        return;
    }

    instrument_ = *di;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxDigitalOptionInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->foreignCurrencyCombo->blockSignals(b);
        ui_->domesticCurrencyCombo->blockSignals(b);
        ui_->payoffCurrencyCombo->blockSignals(b);
        ui_->payoffAmountSpinBox->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->expiryDateEdit->blockSignals(b);
        ui_->longShortCombo->blockSignals(b);
        ui_->strikeSpinBox->blockSignals(b);
        ui_->barrierTypeCombo->blockSignals(b);
        ui_->lowerBarrierSpinBox->blockSignals(b);
        ui_->upperBarrierSpinBox->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    InstrumentFormUtils::setComboValue(ui_->foreignCurrencyCombo,
        instrument_.foreign_currency);
    InstrumentFormUtils::setComboValue(ui_->domesticCurrencyCombo,
        instrument_.domestic_currency);
    InstrumentFormUtils::setComboValue(ui_->payoffCurrencyCombo,
        instrument_.payoff_currency);
    ui_->payoffAmountSpinBox->setValue(instrument_.payoff_amount);
    InstrumentFormUtils::setComboValue(ui_->optionTypeCombo, instrument_.option_type);
    ui_->expiryDateEdit->setIsoDate(instrument_.expiry_date);
    InstrumentFormUtils::setComboValue(ui_->longShortCombo, instrument_.long_short);
    ui_->strikeSpinBox->setValue(instrument_.strike.value_or(0.0));
    InstrumentFormUtils::setComboValue(ui_->barrierTypeCombo, instrument_.barrier_type);
    ui_->lowerBarrierSpinBox->setValue(instrument_.lower_barrier.value_or(0.0));
    ui_->upperBarrierSpinBox->setValue(instrument_.upper_barrier.value_or(0.0));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    block(false);
}

void FxDigitalOptionInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxDigitalOptionInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void FxDigitalOptionInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<FxDigitalOptionInstrumentForm> self = this;
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
                << "FX digital option save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX digital option instrument saved";
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
        trading::messaging::save_fx_digital_option_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
