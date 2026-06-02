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
#include "ores.qt/FxInstrumentForm.hpp"

#include <QComboBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"

namespace ores::qt {

using namespace ores::logging;

FxInstrumentForm::FxInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::FxInstrumentForm) {
    ui_->setupUi(this);
    // Options sub-tab is hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->optionsTab), false);
    InstrumentFormUtils::populateSettlement(ui_->settlementCombo);
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    setupConnections();
}

FxInstrumentForm::~FxInstrumentForm() = default;

void FxInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    auto markChangedStr = [this](const QString&) { onFieldChanged(); };
    auto markChangedDate = [this](const QDate&) { onFieldChanged(); };
    connect(ui_->boughtCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->soldCurrencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->valueDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->settlementCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged,
            this, markChanged);
    connect(ui_->boughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->soldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void FxInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void FxInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->boughtCurrencyCombo, imageCache_,
                     FlagSource::Currency);
    setup_flag_combo(this, ui_->soldCurrencyCombo, imageCache_,
                     FlagSource::Currency);
}

void FxInstrumentForm::populateCurrencies() {
    if (!clientManager_) return;

    QPointer<FxInstrumentForm> self = this;
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

void FxInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxInstrumentForm::clear() {
    instrument_ = trading::domain::fx_forward_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void FxInstrumentForm::setTradeType(const QString& code,
    bool has_options, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
    // FX has only an options sub-section; the extension flag is unused.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->optionsTab), has_options);
}

void FxInstrumentForm::setReadOnly(bool readOnly) {
    ui_->boughtCurrencyCombo->setEnabled(!readOnly);
    ui_->boughtAmountSpinBox->setReadOnly(readOnly);
    ui_->soldCurrencyCombo->setEnabled(!readOnly);
    ui_->soldAmountSpinBox->setReadOnly(readOnly);
    ui_->valueDateEdit->setReadOnly(readOnly);
    ui_->settlementCombo->setEnabled(!readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxInstrumentForm::isDirty() const {
    return dirty_;
}

bool FxInstrumentForm::isLoaded() const {
    return loaded_;
}

void FxInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxInstrumentForm::writeUiToInstrument() {
    instrument_.bought_currency =
        InstrumentFormUtils::getComboValue(ui_->boughtCurrencyCombo);
    instrument_.bought_amount = ui_->boughtAmountSpinBox->value();
    instrument_.sold_currency =
        InstrumentFormUtils::getComboValue(ui_->soldCurrencyCombo);
    instrument_.sold_amount = ui_->soldAmountSpinBox->value();
    instrument_.value_date = ui_->valueDateEdit->isoDate();
    instrument_.settlement =
        InstrumentFormUtils::getComboValue(ui_->settlementCombo);
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxInstrumentForm::populate(
    const trading::domain::fx_forward_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    BOOST_LOG_SEV(lg(), debug)
        << "FxInstrumentForm: instrument loaded trade_type_code="
        << instrument_.trade_type_code;
    emit instrumentLoaded();
}

void FxInstrumentForm::populateFromInstrument() {
    // Block signals while setting values so dirty_ stays untouched.
    const auto block = [this](bool b) {
        ui_->boughtCurrencyCombo->blockSignals(b);
        ui_->boughtAmountSpinBox->blockSignals(b);
        ui_->soldCurrencyCombo->blockSignals(b);
        ui_->soldAmountSpinBox->blockSignals(b);
        ui_->valueDateEdit->blockSignals(b);
        ui_->settlementCombo->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
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
    ui_->valueDateEdit->setIsoDate(instrument_.value_date);
    InstrumentFormUtils::setComboValue(ui_->settlementCombo, instrument_.settlement);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    block(false);
}

void FxInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void FxInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<FxInstrumentForm> self = this;
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
                << "FX instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX instrument saved";
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
        trading::messaging::save_fx_forward_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
