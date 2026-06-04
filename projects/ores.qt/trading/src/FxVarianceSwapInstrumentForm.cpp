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
#include "ores.qt/FxVarianceSwapInstrumentForm.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ui_FxVarianceSwapInstrumentForm.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

FxVarianceSwapInstrumentForm::FxVarianceSwapInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent)
    , ui_(new Ui::FxVarianceSwapInstrumentForm) {
    ui_->setupUi(this);
    InstrumentFormUtils::populateLongShort(ui_->longShortCombo);
    InstrumentFormUtils::populateMomentType(ui_->momentTypeCombo);
    setupConnections();
}

FxVarianceSwapInstrumentForm::~FxVarianceSwapInstrumentForm() = default;

void FxVarianceSwapInstrumentForm::setupConnections() {
    auto markChanged = [this]() {
        onFieldChanged();
    };
    auto markChangedStr = [this](const QString&) {
        onFieldChanged();
    };
    auto markChangedDate = [this](const QDate&) {
        onFieldChanged();
    };
    connect(ui_->startDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->endDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->longShortCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->strikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->momentTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
}

void FxVarianceSwapInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxVarianceSwapInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->currencyCombo, imageCache_, FlagSource::Currency);
}

void FxVarianceSwapInstrumentForm::populateCurrencies() {
    if (!clientManager_)
        return;

    QPointer<FxVarianceSwapInstrumentForm> self = this;
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

void FxVarianceSwapInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxVarianceSwapInstrumentForm::clear() {
    const std::string ttc = instrument_.trade_type_code;
    instrument_ = trading::domain::fx_variance_swap_instrument{};
    instrument_.trade_type_code = ttc;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
}

void FxVarianceSwapInstrumentForm::setTradeType(const QString& code,
                                                bool /*has_options*/,
                                                bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
}

void FxVarianceSwapInstrumentForm::setReadOnly(bool readOnly) {
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->endDateEdit->setReadOnly(readOnly);
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->longShortCombo->setEnabled(!readOnly);
    ui_->strikeSpinBox->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->momentTypeCombo->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxVarianceSwapInstrumentForm::isDirty() const {
    return dirty_;
}
bool FxVarianceSwapInstrumentForm::isLoaded() const {
    return loaded_;
}

void FxVarianceSwapInstrumentForm::setChangeReason(const std::string& code,
                                                   const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxVarianceSwapInstrumentForm::writeUiToInstrument() {
    instrument_.start_date = ui_->startDateEdit->isoDate();
    instrument_.end_date = ui_->endDateEdit->isoDate();
    instrument_.currency = InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    instrument_.underlying_code = ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.long_short = InstrumentFormUtils::getComboValue(ui_->longShortCombo);
    instrument_.strike = ui_->strikeSpinBox->value();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.moment_type = InstrumentFormUtils::getComboValue(ui_->momentTypeCombo);
    instrument_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxVarianceSwapInstrumentForm::populate(
    const trading::domain::fx_variance_swap_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxVarianceSwapInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->startDateEdit->blockSignals(b);
        ui_->endDateEdit->blockSignals(b);
        ui_->currencyCombo->blockSignals(b);
        ui_->underlyingCodeEdit->blockSignals(b);
        ui_->longShortCombo->blockSignals(b);
        ui_->strikeSpinBox->blockSignals(b);
        ui_->notionalSpinBox->blockSignals(b);
        ui_->momentTypeCombo->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(QString::fromStdString(instrument_.trade_type_code));
    ui_->startDateEdit->setIsoDate(instrument_.start_date);
    ui_->endDateEdit->setIsoDate(instrument_.end_date);
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, instrument_.currency);
    ui_->underlyingCodeEdit->setText(QString::fromStdString(instrument_.underlying_code));
    InstrumentFormUtils::setComboValue(ui_->longShortCombo, instrument_.long_short);
    ui_->strikeSpinBox->setValue(instrument_.strike);
    ui_->notionalSpinBox->setValue(instrument_.notional);
    InstrumentFormUtils::setComboValue(ui_->momentTypeCombo, instrument_.moment_type);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(instrument_.description));
    block(false);
}

void FxVarianceSwapInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxVarianceSwapInstrumentForm::onFieldChanged() {
    if (!loaded_)
        return;
    dirty_ = true;
    emit changed();
}

void FxVarianceSwapInstrumentForm::saveInstrument(
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

    QPointer<FxVarianceSwapInstrumentForm> self = this;
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
                BOOST_LOG_SEV(lg(), error) << "FX variance swap save failed: " << result.message;
                on_failure(QString::fromStdString(result.message));
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "FX variance swap instrument saved";
            self->dirty_ = false;
            self->emitProvenance();
            on_success(boost::uuids::to_string(self->instrument_.instrument_id));
        });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run([cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_fx_variance_swap_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
