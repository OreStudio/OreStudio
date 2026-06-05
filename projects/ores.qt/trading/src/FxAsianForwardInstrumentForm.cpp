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
#include "ores.qt/FxAsianForwardInstrumentForm.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ui_FxAsianForwardInstrumentForm.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxAsianForwardInstrumentForm::FxAsianForwardInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent)
    , ui_(new Ui::FxAsianForwardInstrumentForm) {
    ui_->setupUi(this);
    InstrumentFormUtils::populateLongShort(ui_->longShortCombo);
    setupConnections();
}

FxAsianForwardInstrumentForm::~FxAsianForwardInstrumentForm() = default;

void FxAsianForwardInstrumentForm::setupConnections() {
    auto markChanged = [this]() {
        onFieldChanged();
    };
    auto markChangedStr = [this](const QString&) {
        onFieldChanged();
    };
    auto markChangedDate = [this](const QDate&) {
        onFieldChanged();
    };
    connect(ui_->fxIndexEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->referenceCurrencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->referenceNotionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->settlementCurrencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->settlementNotionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->paymentDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->longShortCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->fixingAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->targetAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->strikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void FxAsianForwardInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxAsianForwardInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->referenceCurrencyCombo, imageCache_, FlagSource::Currency);
    setup_flag_combo(this, ui_->settlementCurrencyCombo, imageCache_, FlagSource::Currency);
    setup_flag_combo(this, ui_->currencyCombo, imageCache_, FlagSource::Currency);
}

void FxAsianForwardInstrumentForm::populateCurrencies() {
    if (!clientManager_)
        return;

    QPointer<FxAsianForwardInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        auto populate = [&codes](QComboBox* cb, const std::string& value) {
            cb->blockSignals(true);
            cb->clear();
            cb->addItem(QString());
            for (const auto& c : codes)
                cb->addItem(QString::fromStdString(c));
            InstrumentFormUtils::setComboValue(cb, value);
            cb->blockSignals(false);
        };

        populate(self->ui_->referenceCurrencyCombo, self->instrument_.reference_currency);
        populate(self->ui_->settlementCurrencyCombo, self->instrument_.settlement_currency);
        populate(self->ui_->currencyCombo, self->instrument_.currency);

        if (self->imageCache_) {
            apply_flag_icons(
                self->ui_->referenceCurrencyCombo, self->imageCache_, FlagSource::Currency);
            apply_flag_icons(
                self->ui_->settlementCurrencyCombo, self->imageCache_, FlagSource::Currency);
            apply_flag_icons(self->ui_->currencyCombo, self->imageCache_, FlagSource::Currency);
        }
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void FxAsianForwardInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxAsianForwardInstrumentForm::clear() {
    const std::string ttc = instrument_.identity.trade_type_code;
    instrument_ = trading::domain::fx_asian_forward_instrument{};
    instrument_.identity.trade_type_code = ttc;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
}

void FxAsianForwardInstrumentForm::setTradeType(const QString& code,
                                                bool /*has_options*/,
                                                bool /*has_extension*/) {
    instrument_.identity.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
}

void FxAsianForwardInstrumentForm::setReadOnly(bool readOnly) {
    ui_->fxIndexEdit->setReadOnly(readOnly);
    ui_->referenceCurrencyCombo->setEnabled(!readOnly);
    ui_->referenceNotionalSpinBox->setReadOnly(readOnly);
    ui_->settlementCurrencyCombo->setEnabled(!readOnly);
    ui_->settlementNotionalSpinBox->setReadOnly(readOnly);
    ui_->paymentDateEdit->setReadOnly(readOnly);
    ui_->longShortCombo->setEnabled(!readOnly);
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->fixingAmountSpinBox->setReadOnly(readOnly);
    ui_->targetAmountSpinBox->setReadOnly(readOnly);
    ui_->strikeSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxAsianForwardInstrumentForm::isDirty() const {
    return dirty_;
}
bool FxAsianForwardInstrumentForm::isLoaded() const {
    return loaded_;
}

void FxAsianForwardInstrumentForm::setChangeReason(const std::string& code,
                                                   const std::string& commentary) {
    instrument_.audit.change_reason_code = code;
    instrument_.audit.change_commentary = commentary;
}

void FxAsianForwardInstrumentForm::writeUiToInstrument() {
    instrument_.fx_index = ui_->fxIndexEdit->text().trimmed().toStdString();
    instrument_.reference_currency =
        InstrumentFormUtils::getComboValue(ui_->referenceCurrencyCombo);
    {
        const double v = ui_->referenceNotionalSpinBox->value();
        instrument_.reference_notional = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.settlement_currency =
        InstrumentFormUtils::getComboValue(ui_->settlementCurrencyCombo);
    {
        const double v = ui_->settlementNotionalSpinBox->value();
        instrument_.settlement_notional = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.payment_date = ui_->paymentDateEdit->isoDate();
    instrument_.long_short = InstrumentFormUtils::getComboValue(ui_->longShortCombo);
    instrument_.currency = InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    {
        const double v = ui_->fixingAmountSpinBox->value();
        instrument_.fixing_amount = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->targetAmountSpinBox->value();
        instrument_.target_amount = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->strikeSpinBox->value();
        instrument_.strike = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.audit.modified_by = username_;
    instrument_.audit.performed_by = username_;
}

void FxAsianForwardInstrumentForm::populate(
    const trading::domain::fx_asian_forward_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxAsianForwardInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->fxIndexEdit->blockSignals(b);
        ui_->referenceCurrencyCombo->blockSignals(b);
        ui_->referenceNotionalSpinBox->blockSignals(b);
        ui_->settlementCurrencyCombo->blockSignals(b);
        ui_->settlementNotionalSpinBox->blockSignals(b);
        ui_->paymentDateEdit->blockSignals(b);
        ui_->longShortCombo->blockSignals(b);
        ui_->currencyCombo->blockSignals(b);
        ui_->fixingAmountSpinBox->blockSignals(b);
        ui_->targetAmountSpinBox->blockSignals(b);
        ui_->strikeSpinBox->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(QString::fromStdString(instrument_.identity.trade_type_code));
    ui_->fxIndexEdit->setText(QString::fromStdString(instrument_.fx_index));
    InstrumentFormUtils::setComboValue(ui_->referenceCurrencyCombo, instrument_.reference_currency);
    ui_->referenceNotionalSpinBox->setValue(instrument_.reference_notional.value_or(0.0));
    InstrumentFormUtils::setComboValue(ui_->settlementCurrencyCombo,
                                       instrument_.settlement_currency);
    ui_->settlementNotionalSpinBox->setValue(instrument_.settlement_notional.value_or(0.0));
    ui_->paymentDateEdit->setIsoDate(instrument_.payment_date);
    InstrumentFormUtils::setComboValue(ui_->longShortCombo, instrument_.long_short);
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, instrument_.currency);
    ui_->fixingAmountSpinBox->setValue(instrument_.fixing_amount.value_or(0.0));
    ui_->targetAmountSpinBox->setValue(instrument_.target_amount.value_or(0.0));
    ui_->strikeSpinBox->setValue(instrument_.strike.value_or(0.0));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(instrument_.description));
    block(false);
}

void FxAsianForwardInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.identity.version;
    p.modified_by = instrument_.audit.modified_by;
    p.performed_by = instrument_.audit.performed_by;
    p.recorded_at = instrument_.audit.recorded_at;
    p.change_reason_code = instrument_.audit.change_reason_code;
    p.change_commentary = instrument_.audit.change_commentary;
    emit provenanceChanged(p);
}

void FxAsianForwardInstrumentForm::onFieldChanged() {
    if (!loaded_)
        return;
    dirty_ = true;
    emit changed();
}

void FxAsianForwardInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult {
        bool success;
        std::string message;
    };

    QPointer<FxAsianForwardInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(
        watcher,
        &QFutureWatcher<SaveResult>::finished,
        self,
        [self, watcher, on_success = std::move(on_success), on_failure = std::move(on_failure)]() {
            auto result = watcher->result();
            watcher->deleteLater();
            if (!self)
                return;

            if (!result.success) {
                BOOST_LOG_SEV(lg(), error) << "FX asian forward save failed: " << result.message;
                on_failure(QString::fromStdString(result.message));
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "FX asian forward instrument saved";
            self->dirty_ = false;
            self->emitProvenance();
            on_success(boost::uuids::to_string(self->instrument_.identity.instrument_id));
        });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run([cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_fx_asian_forward_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
