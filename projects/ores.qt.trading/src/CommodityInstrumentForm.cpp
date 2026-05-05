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
#include "ores.qt/CommodityInstrumentForm.hpp"

#include <QComboBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CommodityInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CommodityInstrumentForm::CommodityInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::CommodityInstrumentForm) {
    ui_->setupUi(this);
    // Extensions tab hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), false);
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    InstrumentFormUtils::populateExerciseType(ui_->exerciseTypeCombo);
    InstrumentFormUtils::populateBarrierType(ui_->barrierTypeCombo);
    InstrumentFormUtils::populateAverageType(ui_->averageTypeCombo);
    InstrumentFormUtils::populateFrequency(ui_->stripFrequencyCombo);
    InstrumentFormUtils::populateDayCount(ui_->dayCountCombo);
    InstrumentFormUtils::populateFrequency(ui_->paymentFrequencyCombo);
    setupConnections();
}

CommodityInstrumentForm::~CommodityInstrumentForm() = default;

void CommodityInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    auto markChangedStr = [this](const QString&) { onFieldChanged(); };
    auto markChangedDate = [this](const QDate&) { onFieldChanged(); };
    connect(ui_->commodityCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->unitEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->startDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->maturityDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->optionTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->exerciseTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->barrierTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->averageTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->averagingStartDateEdit, &QDateEdit::dateChanged,
            this, markChangedDate);
    connect(ui_->averagingEndDateEdit, &QDateEdit::dateChanged,
            this, markChangedDate);
    connect(ui_->spreadCommodityCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->stripFrequencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->dayCountCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->paymentFrequencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->swaptionExpiryDateEdit, &QDateEdit::dateChanged,
            this, markChangedDate);
    connect(ui_->basketJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->quantitySpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->fixedPriceSpinBox,
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
    connect(ui_->spreadAmountSpinBox,
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

void CommodityInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
    populateCurrencies();
}

void CommodityInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->currencyCombo, imageCache_,
                     FlagSource::Currency);
}

void CommodityInstrumentForm::populateCurrencies() {
    if (!clientManager_) return;

    QPointer<CommodityInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<std::vector<std::string>>(self);
    connect(watcher, &QFutureWatcher<std::vector<std::string>>::finished, self,
        [self, watcher]() {
        auto codes = watcher->result();
        watcher->deleteLater();
        if (!self) return;
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
    watcher->setFuture(QtConcurrent::run([cm]() {
        return fetch_currency_codes(cm);
    }));
}

void CommodityInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void CommodityInstrumentForm::clear() {
    instrument_ = trading::domain::commodity_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void CommodityInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool has_extension) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void CommodityInstrumentForm::setReadOnly(bool readOnly) {
    ui_->commodityCodeEdit->setReadOnly(readOnly);
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->quantitySpinBox->setReadOnly(readOnly);
    ui_->unitEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->fixedPriceSpinBox->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->exerciseTypeCombo->setEnabled(!readOnly);
    ui_->strikePriceSpinBox->setReadOnly(readOnly);
    ui_->barrierTypeCombo->setEnabled(!readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->averageTypeCombo->setEnabled(!readOnly);
    ui_->averagingStartDateEdit->setReadOnly(readOnly);
    ui_->averagingEndDateEdit->setReadOnly(readOnly);
    ui_->spreadCommodityCodeEdit->setReadOnly(readOnly);
    ui_->spreadAmountSpinBox->setReadOnly(readOnly);
    ui_->stripFrequencyCombo->setEnabled(!readOnly);
    ui_->varianceStrikeSpinBox->setReadOnly(readOnly);
    ui_->accumulationAmountSpinBox->setReadOnly(readOnly);
    ui_->knockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->dayCountCombo->setEnabled(!readOnly);
    ui_->paymentFrequencyCombo->setEnabled(!readOnly);
    ui_->swaptionExpiryDateEdit->setReadOnly(readOnly);
    ui_->basketJsonEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool CommodityInstrumentForm::isDirty() const { return dirty_; }
bool CommodityInstrumentForm::isLoaded() const { return loaded_; }

void CommodityInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void CommodityInstrumentForm::writeUiToInstrument() {
    instrument_.commodity_code =
        ui_->commodityCodeEdit->text().trimmed().toStdString();
    instrument_.currency =
        InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    instrument_.quantity = ui_->quantitySpinBox->value();
    instrument_.unit =
        ui_->unitEdit->text().trimmed().toStdString();
    instrument_.start_date = ui_->startDateEdit->isoDate();
    instrument_.maturity_date = ui_->maturityDateEdit->isoDate();
    {
        const double v = ui_->fixedPriceSpinBox->value();
        instrument_.fixed_price = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.option_type =
        InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    {
        const double v = ui_->strikePriceSpinBox->value();
        instrument_.strike_price = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.exercise_type =
        InstrumentFormUtils::getComboValue(ui_->exerciseTypeCombo);
    instrument_.average_type =
        InstrumentFormUtils::getComboValue(ui_->averageTypeCombo);
    instrument_.averaging_start_date = ui_->averagingStartDateEdit->isoDate();
    instrument_.averaging_end_date = ui_->averagingEndDateEdit->isoDate();
    instrument_.spread_commodity_code =
        ui_->spreadCommodityCodeEdit->text().trimmed().toStdString();
    {
        const double v = ui_->spreadAmountSpinBox->value();
        instrument_.spread_amount = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.strip_frequency_code =
        InstrumentFormUtils::getComboValue(ui_->stripFrequencyCombo);
    {
        const double v = ui_->varianceStrikeSpinBox->value();
        instrument_.variance_strike = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->accumulationAmountSpinBox->value();
        instrument_.accumulation_amount = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->knockOutBarrierSpinBox->value();
        instrument_.knock_out_barrier = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.barrier_type =
        InstrumentFormUtils::getComboValue(ui_->barrierTypeCombo);
    {
        const double v = ui_->lowerBarrierSpinBox->value();
        instrument_.lower_barrier = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    {
        const double v = ui_->upperBarrierSpinBox->value();
        instrument_.upper_barrier = (v > 0.0)
            ? std::optional<double>(v) : std::nullopt;
    }
    instrument_.basket_json =
        ui_->basketJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.day_count_code =
        InstrumentFormUtils::getComboValue(ui_->dayCountCombo);
    instrument_.payment_frequency_code =
        InstrumentFormUtils::getComboValue(ui_->paymentFrequencyCombo);
    instrument_.swaption_expiry_date = ui_->swaptionExpiryDateEdit->isoDate();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CommodityInstrumentForm::setInstrument(
    const trading::domain::trade_instrument& instrument) {

    const auto* ex =
        std::get_if<trading::domain::commodity_instrument>(&instrument);
    if (!ex) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-commodity instrument pushed to CommodityInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for commodity form"));
        return;
    }

    instrument_ = *ex;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void CommodityInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->commodityCodeEdit->blockSignals(b);
        ui_->currencyCombo->blockSignals(b);
        ui_->quantitySpinBox->blockSignals(b);
        ui_->unitEdit->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->fixedPriceSpinBox->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->strikePriceSpinBox->blockSignals(b);
        ui_->exerciseTypeCombo->blockSignals(b);
        ui_->averageTypeCombo->blockSignals(b);
        ui_->averagingStartDateEdit->blockSignals(b);
        ui_->averagingEndDateEdit->blockSignals(b);
        ui_->spreadCommodityCodeEdit->blockSignals(b);
        ui_->spreadAmountSpinBox->blockSignals(b);
        ui_->stripFrequencyCombo->blockSignals(b);
        ui_->varianceStrikeSpinBox->blockSignals(b);
        ui_->accumulationAmountSpinBox->blockSignals(b);
        ui_->knockOutBarrierSpinBox->blockSignals(b);
        ui_->barrierTypeCombo->blockSignals(b);
        ui_->lowerBarrierSpinBox->blockSignals(b);
        ui_->upperBarrierSpinBox->blockSignals(b);
        ui_->basketJsonEdit->blockSignals(b);
        ui_->dayCountCombo->blockSignals(b);
        ui_->paymentFrequencyCombo->blockSignals(b);
        ui_->swaptionExpiryDateEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->commodityCodeEdit->setText(
        QString::fromStdString(instrument_.commodity_code));
    InstrumentFormUtils::setComboValue(
        ui_->currencyCombo, instrument_.currency);
    ui_->quantitySpinBox->setValue(instrument_.quantity);
    ui_->unitEdit->setText(
        QString::fromStdString(instrument_.unit));
    ui_->startDateEdit->setIsoDate(instrument_.start_date);
    ui_->maturityDateEdit->setIsoDate(instrument_.maturity_date);
    ui_->fixedPriceSpinBox->setValue(instrument_.fixed_price.value_or(0.0));
    InstrumentFormUtils::setComboValue(
        ui_->optionTypeCombo, instrument_.option_type);
    ui_->strikePriceSpinBox->setValue(instrument_.strike_price.value_or(0.0));
    InstrumentFormUtils::setComboValue(
        ui_->exerciseTypeCombo, instrument_.exercise_type);
    InstrumentFormUtils::setComboValue(
        ui_->averageTypeCombo, instrument_.average_type);
    ui_->averagingStartDateEdit->setIsoDate(instrument_.averaging_start_date);
    ui_->averagingEndDateEdit->setIsoDate(instrument_.averaging_end_date);
    ui_->spreadCommodityCodeEdit->setText(
        QString::fromStdString(instrument_.spread_commodity_code));
    ui_->spreadAmountSpinBox->setValue(instrument_.spread_amount.value_or(0.0));
    InstrumentFormUtils::setComboValue(
        ui_->stripFrequencyCombo, instrument_.strip_frequency_code);
    ui_->varianceStrikeSpinBox->setValue(
        instrument_.variance_strike.value_or(0.0));
    ui_->accumulationAmountSpinBox->setValue(
        instrument_.accumulation_amount.value_or(0.0));
    ui_->knockOutBarrierSpinBox->setValue(
        instrument_.knock_out_barrier.value_or(0.0));
    InstrumentFormUtils::setComboValue(
        ui_->barrierTypeCombo, instrument_.barrier_type);
    ui_->lowerBarrierSpinBox->setValue(instrument_.lower_barrier.value_or(0.0));
    ui_->upperBarrierSpinBox->setValue(instrument_.upper_barrier.value_or(0.0));
    ui_->basketJsonEdit->setPlainText(
        QString::fromStdString(instrument_.basket_json));
    InstrumentFormUtils::setComboValue(
        ui_->dayCountCombo, instrument_.day_count_code);
    InstrumentFormUtils::setComboValue(
        ui_->paymentFrequencyCombo, instrument_.payment_frequency_code);
    ui_->swaptionExpiryDateEdit->setIsoDate(instrument_.swaption_expiry_date);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    block(false);
}

void CommodityInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void CommodityInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void CommodityInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<CommodityInstrumentForm> self = this;
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
                << "Commodity instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Commodity instrument saved";
        self->dirty_ = false;
        self->emitProvenance();
        on_success(boost::uuids::to_string(self->instrument_.id));
    });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    watcher->setFuture(QtConcurrent::run(
        [cm, instrument = std::move(instrument)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_commodity_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
