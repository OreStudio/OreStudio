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
#include "ores.trading.api/messaging/bond_instrument_protocol.hpp"
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
    connect(ui_->couponFrequencyCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->dayCountCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->optionExpiryDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->trsReturnTypeCombo, &QComboBox::currentTextChanged, this, markChangedStr);
    connect(ui_->trsFundingLegCodeEdit, &QLineEdit::textChanged, this, markChanged);
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
        InstrumentFormUtils::setComboValue(cb, self->data_.issue.currency);
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
    data_ = trading::domain::bond_instrument_data{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void BondInstrumentForm::setTradeType(const QString& code,
                                      bool /*has_options*/,
                                      bool has_extension) {
    data_.instrument.identity.trade_type_code = code.trimmed().toStdString();
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
    ui_->settlementDaysSpinBox->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->optionExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionStrikeSpinBox->setReadOnly(readOnly);
    ui_->trsReturnTypeCombo->setEnabled(!readOnly);
    ui_->trsFundingLegCodeEdit->setReadOnly(readOnly);
}

bool BondInstrumentForm::isDirty() const {
    return dirty_;
}
bool BondInstrumentForm::isLoaded() const {
    return loaded_;
}

void BondInstrumentForm::setChangeReason(const std::string& code, const std::string& commentary) {
    data_.instrument.audit.change_reason_code = code;
    data_.instrument.audit.change_commentary = commentary;
}

void BondInstrumentForm::writeUiToInstrument() {
    data_.issue.issuer = ui_->issuerEdit->text().trimmed().toStdString();
    data_.issue.currency = InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    data_.issue.face_value = ui_->faceValueSpinBox->value();
    data_.issue.coupon_rate = ui_->couponRateSpinBox->value();
    data_.issue.coupon_frequency_code =
        InstrumentFormUtils::getComboValue(ui_->couponFrequencyCombo);
    data_.issue.day_count_code = InstrumentFormUtils::getComboValue(ui_->dayCountCombo);
    data_.issue.issue_date = ui_->issueDateEdit->isoDate();
    data_.issue.settlement_days = ui_->settlementDaysSpinBox->value();
    // The form carries one exercise date; the container carries the
    // whole list a document can hold. The reworked form is unit 3.
    const std::string option_expiry_date = ui_->optionExpiryDateEdit->isoDate();
    data_.option_exercise_dates.clear();
    if (!option_expiry_date.empty())
        data_.option_exercise_dates.push_back(option_expiry_date);

    // The fact rows engage only when their combo carries a value, and an
    // emptied combo clears the staged fact so the container tracks the
    // form; the loaded container decides which product rows this trade
    // has, and an empty extensions page must not fabricate a fact for a
    // plain bond.
    const std::string option_type = InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    if (option_type.empty()) {
        data_.option = std::nullopt;
    } else {
        if (!data_.option)
            data_.option.emplace();
        data_.option->option_type = option_type;
        data_.option->option_strike = ui_->optionStrikeSpinBox->value();
    }
    const std::string trs_return_type = InstrumentFormUtils::getComboValue(ui_->trsReturnTypeCombo);
    if (trs_return_type.empty()) {
        data_.trs = std::nullopt;
    } else {
        if (!data_.trs)
            data_.trs.emplace();
        data_.trs->return_type = trs_return_type;
        data_.trs->funding_index = ui_->trsFundingLegCodeEdit->text().trimmed().toStdString();
    }

    data_.instrument.audit.modified_by = username_;
    data_.instrument.audit.performed_by = username_;
}

void BondInstrumentForm::populate(const trading::domain::bond_instrument_data& data) {
    data_ = data;
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
        ui_->settlementDaysSpinBox->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->optionExpiryDateEdit->blockSignals(b);
        ui_->optionStrikeSpinBox->blockSignals(b);
        ui_->trsReturnTypeCombo->blockSignals(b);
        ui_->trsFundingLegCodeEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(data_.instrument.identity.trade_type_code));
    ui_->issuerEdit->setText(QString::fromStdString(data_.issue.issuer));
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, data_.issue.currency);
    ui_->faceValueSpinBox->setValue(data_.issue.face_value);
    ui_->couponRateSpinBox->setValue(data_.issue.coupon_rate);
    InstrumentFormUtils::setComboValue(ui_->couponFrequencyCombo,
                                       data_.issue.coupon_frequency_code);
    InstrumentFormUtils::setComboValue(ui_->dayCountCombo, data_.issue.day_count_code);
    ui_->issueDateEdit->setIsoDate(data_.issue.issue_date);
    ui_->settlementDaysSpinBox->setValue(data_.issue.settlement_days);
    ui_->optionExpiryDateEdit->setIsoDate(data_.option_exercise_dates.empty()
                                              ? std::string()
                                              : data_.option_exercise_dates.front());
    const bool has_option = data_.option.has_value();
    InstrumentFormUtils::setComboValue(ui_->optionTypeCombo,
                                       has_option ? data_.option->option_type : std::string());
    ui_->optionStrikeSpinBox->setValue(has_option ? data_.option->option_strike : 0.0);
    const bool has_trs = data_.trs.has_value();
    InstrumentFormUtils::setComboValue(ui_->trsReturnTypeCombo,
                                       has_trs ? data_.trs->return_type : std::string());
    ui_->trsFundingLegCodeEdit->setText(has_trs ? QString::fromStdString(data_.trs->funding_index) :
                                                  QString());
    block(false);
}

void BondInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = data_.instrument.identity.version;
    p.modified_by = data_.instrument.audit.modified_by;
    p.performed_by = data_.instrument.audit.performed_by;
    p.recorded_at = data_.instrument.audit.recorded_at;
    p.change_reason_code = data_.instrument.audit.change_reason_code;
    p.change_commentary = data_.instrument.audit.change_commentary;
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
            on_success(boost::uuids::to_string(self->data_.instrument.identity.instrument_id));
        });

    // The wave saves the slim header row only: issue and fact edits ride
    // the in-memory container until the qt bridge ships the parts save.
    auto* cm = clientManager_;
    auto instrument = data_.instrument;
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
