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
#include "ores.qt/FxAccumulatorInstrumentForm.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ui_FxAccumulatorInstrumentForm.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxAccumulatorInstrumentForm::FxAccumulatorInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent)
    , ui_(new Ui::FxAccumulatorInstrumentForm) {
    ui_->setupUi(this);
    InstrumentFormUtils::populateLongShort(ui_->longShortCombo);
    setupConnections();
}

FxAccumulatorInstrumentForm::~FxAccumulatorInstrumentForm() = default;

void FxAccumulatorInstrumentForm::setupConnections() {
    auto markChanged = [this]() {
        onFieldChanged();
    };
    auto markChangedStr = [this](const QString&) {
        onFieldChanged();
    };
    auto markChangedDate = [this](const QDate&) {
        onFieldChanged();
    };
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->fixingAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->strikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->longShortCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->startDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->knockOutBarrierSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void FxAccumulatorInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxAccumulatorInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->currencyCombo, imageCache_, FlagSource::Currency);
}

void FxAccumulatorInstrumentForm::populateCurrencies() {
    if (!clientManager_)
        return;

    QPointer<FxAccumulatorInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self, [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self)
            return;

        auto* cb = self->ui_->currencyCombo;
        cb->blockSignals(true);
        cb->clear();
        cb->addItem(QString());
        for (const auto& c : codes)
            cb->addItem(QString::fromStdString(c));
        InstrumentFormUtils::setComboValue(cb, self->instrument_.currency);
        cb->blockSignals(false);
        if (self->imageCache_)
            apply_flag_icons(cb, self->imageCache_, FlagSource::Currency);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void FxAccumulatorInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxAccumulatorInstrumentForm::clear() {
    const std::string ttc = instrument_.trade_type_code;
    instrument_ = trading::domain::fx_accumulator_instrument{};
    instrument_.trade_type_code = ttc;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
}

void FxAccumulatorInstrumentForm::setTradeType(const QString& code,
                                               bool /*has_options*/,
                                               bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
}

void FxAccumulatorInstrumentForm::setReadOnly(bool readOnly) {
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->fixingAmountSpinBox->setReadOnly(readOnly);
    ui_->strikeSpinBox->setReadOnly(readOnly);
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->longShortCombo->setEnabled(!readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->knockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxAccumulatorInstrumentForm::isDirty() const {
    return dirty_;
}
bool FxAccumulatorInstrumentForm::isLoaded() const {
    return loaded_;
}

void FxAccumulatorInstrumentForm::setChangeReason(const std::string& code,
                                                  const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxAccumulatorInstrumentForm::writeUiToInstrument() {
    instrument_.currency = InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    instrument_.fixing_amount = ui_->fixingAmountSpinBox->value();
    instrument_.strike = ui_->strikeSpinBox->value();
    instrument_.underlying_code = ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.long_short = InstrumentFormUtils::getComboValue(ui_->longShortCombo);
    instrument_.start_date = ui_->startDateEdit->isoDate();
    {
        const double v = ui_->knockOutBarrierSpinBox->value();
        instrument_.knock_out_barrier = (v > 0.0) ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxAccumulatorInstrumentForm::populate(
    const trading::domain::fx_accumulator_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxAccumulatorInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->currencyCombo->blockSignals(b);
        ui_->fixingAmountSpinBox->blockSignals(b);
        ui_->strikeSpinBox->blockSignals(b);
        ui_->underlyingCodeEdit->blockSignals(b);
        ui_->longShortCombo->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->knockOutBarrierSpinBox->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(QString::fromStdString(instrument_.trade_type_code));
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, instrument_.currency);
    ui_->fixingAmountSpinBox->setValue(instrument_.fixing_amount);
    ui_->strikeSpinBox->setValue(instrument_.strike);
    ui_->underlyingCodeEdit->setText(QString::fromStdString(instrument_.underlying_code));
    InstrumentFormUtils::setComboValue(ui_->longShortCombo, instrument_.long_short);
    ui_->startDateEdit->setIsoDate(instrument_.start_date);
    ui_->knockOutBarrierSpinBox->setValue(instrument_.knock_out_barrier.value_or(0.0));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(instrument_.description));
    block(false);
}

void FxAccumulatorInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxAccumulatorInstrumentForm::onFieldChanged() {
    if (!loaded_)
        return;
    dirty_ = true;
    emit changed();
}

void FxAccumulatorInstrumentForm::saveInstrument(std::function<void(const std::string&)> on_success,
                                                 std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult {
        bool success;
        std::string message;
    };

    QPointer<FxAccumulatorInstrumentForm> self = this;
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
                BOOST_LOG_SEV(lg(), error) << "FX accumulator save failed: " << result.message;
                on_failure(QString::fromStdString(result.message));
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "FX accumulator instrument saved";
            self->dirty_ = false;
            self->emitProvenance();
            on_success(boost::uuids::to_string(self->instrument_.instrument_id));
        });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run([cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_fx_accumulator_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
