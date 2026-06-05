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
#include "ores.qt/BondInstrumentForm.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"
#include "ui_BondInstrumentForm.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QPointer>
#include <QtConcurrent>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

BondInstrumentForm::BondInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent)
    , ui_(new Ui::BondInstrumentForm) {
    ui_->setupUi(this);
    // Extensions tab is hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(ui_->subTabWidget->indexOf(ui_->extensionsTab), false);
    InstrumentFormUtils::populateFrequency(ui_->couponFrequencyCombo);
    InstrumentFormUtils::populateDayCount(ui_->dayCountCombo);
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    InstrumentFormUtils::populateTrsReturnType(ui_->trsReturnTypeCombo);
    InstrumentFormUtils::populateAscotOptionType(ui_->ascotOptionTypeCombo);
    setupConnections();
}

BondInstrumentForm::~BondInstrumentForm() = default;

void BondInstrumentForm::setupConnections() {
    auto markChanged = [this]() {
        onFieldChanged();
    };
    auto markChangedStr = [this](const QString&) {
        onFieldChanged();
    };
    auto markChangedDate = [this](const QDate&) {
        onFieldChanged();
    };
    connect(ui_->issuerEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->issueDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->maturityDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->couponFrequencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->dayCountCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->callDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->futureExpiryDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->optionExpiryDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->trsReturnTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->trsFundingLegCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->ascotOptionTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->faceValueSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->couponRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(
        ui_->settlementDaysSpinBox, QOverload<int>::of(&QSpinBox::valueChanged), this, markChanged);
    connect(ui_->conversionRatioSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
    connect(ui_->optionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this,
            markChanged);
}

void BondInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void BondInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->currencyCombo, imageCache_, FlagSource::Currency);
}

void BondInstrumentForm::populateCurrencies() {
    if (!clientManager_)
        return;

    QPointer<BondInstrumentForm> self = this;
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
        InstrumentFormUtils::setComboValue(cb, self->instrument_.terms.currency);
        cb->blockSignals(false);
        if (self->imageCache_)
            apply_flag_icons(cb, self->imageCache_, FlagSource::Currency);
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm]() { return fetch_currency_codes(cm); }));
}

void BondInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void BondInstrumentForm::clear() {
    instrument_ = trading::domain::bond_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void BondInstrumentForm::setTradeType(const QString& code,
                                      bool /*has_options*/,
                                      bool has_extension) {
    instrument_.identity.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
    ui_->subTabWidget->setTabVisible(ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void BondInstrumentForm::setReadOnly(bool readOnly) {
    ui_->issuerEdit->setReadOnly(readOnly);
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->faceValueSpinBox->setReadOnly(readOnly);
    ui_->couponRateSpinBox->setReadOnly(readOnly);
    ui_->couponFrequencyCombo->setEnabled(!readOnly);
    ui_->dayCountCombo->setEnabled(!readOnly);
    ui_->issueDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->settlementDaysSpinBox->setReadOnly(readOnly);
    ui_->callDateEdit->setReadOnly(readOnly);
    ui_->conversionRatioSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->futureExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->optionExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionStrikeSpinBox->setReadOnly(readOnly);
    ui_->trsReturnTypeCombo->setEnabled(!readOnly);
    ui_->trsFundingLegCodeEdit->setReadOnly(readOnly);
    ui_->ascotOptionTypeCombo->setEnabled(!readOnly);
}

bool BondInstrumentForm::isDirty() const {
    return dirty_;
}
bool BondInstrumentForm::isLoaded() const {
    return loaded_;
}

void BondInstrumentForm::setChangeReason(const std::string& code, const std::string& commentary) {
    instrument_.audit.change_reason_code = code;
    instrument_.audit.change_commentary = commentary;
}

void BondInstrumentForm::writeUiToInstrument() {
    instrument_.terms.issuer = ui_->issuerEdit->text().trimmed().toStdString();
    instrument_.terms.currency = InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    instrument_.terms.face_value = ui_->faceValueSpinBox->value();
    instrument_.terms.coupon_rate = ui_->couponRateSpinBox->value();
    instrument_.terms.coupon_frequency_code =
        InstrumentFormUtils::getComboValue(ui_->couponFrequencyCombo);
    instrument_.terms.day_count_code = InstrumentFormUtils::getComboValue(ui_->dayCountCombo);
    instrument_.terms.issue_date = ui_->issueDateEdit->isoDate();
    instrument_.terms.maturity_date = ui_->maturityDateEdit->isoDate();
    instrument_.features.settlement_days = ui_->settlementDaysSpinBox->value();
    instrument_.features.call_date = ui_->callDateEdit->isoDate();
    instrument_.features.conversion_ratio = ui_->conversionRatioSpinBox->value();
    instrument_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.features.future_expiry_date = ui_->futureExpiryDateEdit->isoDate();
    instrument_.option.option_type = InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    instrument_.option.option_expiry_date = ui_->optionExpiryDateEdit->isoDate();
    {
        const double s = ui_->optionStrikeSpinBox->value();
        instrument_.option.option_strike = (s > 0.0) ? std::optional<double>(s) : std::nullopt;
    }
    instrument_.features.trs_return_type =
        InstrumentFormUtils::getComboValue(ui_->trsReturnTypeCombo);
    instrument_.features.trs_funding_leg_code =
        ui_->trsFundingLegCodeEdit->text().trimmed().toStdString();
    instrument_.option.ascot_option_type =
        InstrumentFormUtils::getComboValue(ui_->ascotOptionTypeCombo);
    instrument_.audit.modified_by = username_;
    instrument_.audit.performed_by = username_;
}

void BondInstrumentForm::populate(const trading::domain::bond_instrument& instr) {
    instrument_ = instr;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void BondInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->issuerEdit->blockSignals(b);
        ui_->currencyCombo->blockSignals(b);
        ui_->faceValueSpinBox->blockSignals(b);
        ui_->couponRateSpinBox->blockSignals(b);
        ui_->couponFrequencyCombo->blockSignals(b);
        ui_->dayCountCombo->blockSignals(b);
        ui_->issueDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->settlementDaysSpinBox->blockSignals(b);
        ui_->callDateEdit->blockSignals(b);
        ui_->conversionRatioSpinBox->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->futureExpiryDateEdit->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->optionExpiryDateEdit->blockSignals(b);
        ui_->optionStrikeSpinBox->blockSignals(b);
        ui_->trsReturnTypeCombo->blockSignals(b);
        ui_->trsFundingLegCodeEdit->blockSignals(b);
        ui_->ascotOptionTypeCombo->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(QString::fromStdString(instrument_.identity.trade_type_code));
    ui_->issuerEdit->setText(QString::fromStdString(instrument_.terms.issuer));
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, instrument_.terms.currency);
    ui_->faceValueSpinBox->setValue(instrument_.terms.face_value);
    ui_->couponRateSpinBox->setValue(instrument_.terms.coupon_rate);
    InstrumentFormUtils::setComboValue(ui_->couponFrequencyCombo,
                                       instrument_.terms.coupon_frequency_code);
    InstrumentFormUtils::setComboValue(ui_->dayCountCombo, instrument_.terms.day_count_code);
    ui_->issueDateEdit->setIsoDate(instrument_.terms.issue_date);
    ui_->maturityDateEdit->setIsoDate(instrument_.terms.maturity_date);
    ui_->settlementDaysSpinBox->setValue(instrument_.features.settlement_days);
    ui_->callDateEdit->setIsoDate(instrument_.features.call_date);
    ui_->conversionRatioSpinBox->setValue(instrument_.features.conversion_ratio);
    ui_->descriptionEdit->setPlainText(QString::fromStdString(instrument_.description));
    ui_->futureExpiryDateEdit->setIsoDate(instrument_.features.future_expiry_date);
    InstrumentFormUtils::setComboValue(ui_->optionTypeCombo, instrument_.option.option_type);
    ui_->optionExpiryDateEdit->setIsoDate(instrument_.option.option_expiry_date);
    ui_->optionStrikeSpinBox->setValue(instrument_.option.option_strike.value_or(0.0));
    InstrumentFormUtils::setComboValue(ui_->trsReturnTypeCombo,
                                       instrument_.features.trs_return_type);
    ui_->trsFundingLegCodeEdit->setText(
        QString::fromStdString(instrument_.features.trs_funding_leg_code));
    InstrumentFormUtils::setComboValue(ui_->ascotOptionTypeCombo,
                                       instrument_.option.ascot_option_type);
    block(false);
}

void BondInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.identity.version;
    p.modified_by = instrument_.audit.modified_by;
    p.performed_by = instrument_.audit.performed_by;
    p.recorded_at = instrument_.audit.recorded_at;
    p.change_reason_code = instrument_.audit.change_reason_code;
    p.change_commentary = instrument_.audit.change_commentary;
    emit provenanceChanged(p);
}

void BondInstrumentForm::onFieldChanged() {
    if (!loaded_)
        return;
    dirty_ = true;
    emit changed();
}

void BondInstrumentForm::saveInstrument(std::function<void(const std::string&)> on_success,
                                        std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult {
        bool success;
        std::string message;
    };

    QPointer<BondInstrumentForm> self = this;
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
                BOOST_LOG_SEV(lg(), error) << "Bond instrument save failed: " << result.message;
                on_failure(QString::fromStdString(result.message));
                return;
            }

            BOOST_LOG_SEV(lg(), info) << "Bond instrument saved";
            self->dirty_ = false;
            self->emitProvenance();
            on_success(boost::uuids::to_string(self->instrument_.identity.instrument_id));
        });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run([cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_bond_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
