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

#include <QComboBox>
#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_EquityInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/InstrumentFormUtils.hpp"
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
    InstrumentFormUtils::populateOptionType(ui_->optionTypeCombo);
    InstrumentFormUtils::populateExerciseType(ui_->exerciseTypeCombo);
    InstrumentFormUtils::populateBarrierType(ui_->barrierTypeCombo);
    InstrumentFormUtils::populateAverageType(ui_->averageTypeCombo);
    InstrumentFormUtils::populateFrequency(ui_->cliquetFrequencyCombo);
    InstrumentFormUtils::populateDayCount(ui_->dayCountCombo);
    InstrumentFormUtils::populateFrequency(ui_->paymentFrequencyCombo);
    InstrumentFormUtils::populateReturnType(ui_->returnTypeCombo);
    setupConnections();
}

EquityInstrumentForm::~EquityInstrumentForm() = default;

void EquityInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    auto markChangedStr = [this](const QString&) { onFieldChanged(); };
    auto markChangedDate = [this](const QDate&) { onFieldChanged(); };
    connect(ui_->underlyingCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->startDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->maturityDateEdit, &QDateEdit::dateChanged, this, markChangedDate);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
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
    connect(ui_->cliquetFrequencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->dayCountCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->paymentFrequencyCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
    connect(ui_->returnTypeCombo, &QComboBox::currentTextChanged,
            this, markChangedStr);
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
    populateCurrencies();
}

void EquityInstrumentForm::setImageCache(ImageCache* cache) {
    imageCache_ = cache;
    setup_flag_combo(this, ui_->currencyCombo, imageCache_,
                     FlagSource::Currency);
}

void EquityInstrumentForm::populateCurrencies() {
    if (!clientManager_) return;

    QPointer<EquityInstrumentForm> self = this;
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
    ui_->tradeTypeCodeEdit->setText(code.trimmed());
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->optionsTab), has_options);
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void EquityInstrumentForm::setReadOnly(bool readOnly) {
    ui_->underlyingCodeEdit->setReadOnly(readOnly);
    ui_->currencyCombo->setEnabled(!readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->quantitySpinBox->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->optionTypeCombo->setEnabled(!readOnly);
    ui_->exerciseTypeCombo->setEnabled(!readOnly);
    ui_->strikePriceSpinBox->setReadOnly(readOnly);
    ui_->barrierTypeCombo->setEnabled(!readOnly);
    ui_->lowerBarrierSpinBox->setReadOnly(readOnly);
    ui_->upperBarrierSpinBox->setReadOnly(readOnly);
    ui_->averageTypeCombo->setEnabled(!readOnly);
    ui_->averagingStartDateEdit->setReadOnly(readOnly);
    ui_->varianceStrikeSpinBox->setReadOnly(readOnly);
    ui_->cliquetFrequencyCombo->setEnabled(!readOnly);
    ui_->accumulationAmountSpinBox->setReadOnly(readOnly);
    ui_->knockOutBarrierSpinBox->setReadOnly(readOnly);
    ui_->dayCountCombo->setEnabled(!readOnly);
    ui_->paymentFrequencyCombo->setEnabled(!readOnly);
    ui_->returnTypeCombo->setEnabled(!readOnly);
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
    instrument_.underlying_name =
        ui_->underlyingCodeEdit->text().trimmed().toStdString();
    instrument_.currency =
        InstrumentFormUtils::getComboValue(ui_->currencyCombo);
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.expiry_date = ui_->maturityDateEdit->isoDate();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.option_type =
        InstrumentFormUtils::getComboValue(ui_->optionTypeCombo);
    instrument_.exercise_type =
        InstrumentFormUtils::getComboValue(ui_->exerciseTypeCombo);
    instrument_.strike = ui_->strikePriceSpinBox->value();
    instrument_.cliquet_frequency =
        InstrumentFormUtils::getComboValue(ui_->cliquetFrequencyCombo);
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
        ui_->underlyingCodeEdit->blockSignals(b);
        ui_->currencyCombo->blockSignals(b);
        ui_->notionalSpinBox->blockSignals(b);
        ui_->quantitySpinBox->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->optionTypeCombo->blockSignals(b);
        ui_->exerciseTypeCombo->blockSignals(b);
        ui_->strikePriceSpinBox->blockSignals(b);
        ui_->barrierTypeCombo->blockSignals(b);
        ui_->lowerBarrierSpinBox->blockSignals(b);
        ui_->upperBarrierSpinBox->blockSignals(b);
        ui_->averageTypeCombo->blockSignals(b);
        ui_->averagingStartDateEdit->blockSignals(b);
        ui_->varianceStrikeSpinBox->blockSignals(b);
        ui_->cliquetFrequencyCombo->blockSignals(b);
        ui_->accumulationAmountSpinBox->blockSignals(b);
        ui_->knockOutBarrierSpinBox->blockSignals(b);
        ui_->dayCountCombo->blockSignals(b);
        ui_->paymentFrequencyCombo->blockSignals(b);
        ui_->returnTypeCombo->blockSignals(b);
        ui_->basketJsonEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->underlyingCodeEdit->setText(
        QString::fromStdString(instrument_.underlying_name));
    InstrumentFormUtils::setComboValue(ui_->currencyCombo, instrument_.currency);
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->quantitySpinBox->setValue(0.0);
    ui_->startDateEdit->setIsoDate("");
    ui_->maturityDateEdit->setIsoDate(instrument_.expiry_date);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    InstrumentFormUtils::setComboValue(
        ui_->optionTypeCombo, instrument_.option_type);
    InstrumentFormUtils::setComboValue(
        ui_->exerciseTypeCombo, instrument_.exercise_type);
    ui_->strikePriceSpinBox->setValue(instrument_.strike);
    InstrumentFormUtils::setComboValue(ui_->barrierTypeCombo, "");
    ui_->lowerBarrierSpinBox->setValue(0.0);
    ui_->upperBarrierSpinBox->setValue(0.0);
    InstrumentFormUtils::setComboValue(ui_->averageTypeCombo, "");
    ui_->averagingStartDateEdit->setIsoDate("");
    ui_->varianceStrikeSpinBox->setValue(0.0);
    InstrumentFormUtils::setComboValue(
        ui_->cliquetFrequencyCombo, instrument_.cliquet_frequency);
    ui_->accumulationAmountSpinBox->setValue(0.0);
    ui_->knockOutBarrierSpinBox->setValue(0.0);
    InstrumentFormUtils::setComboValue(ui_->dayCountCombo, "");
    InstrumentFormUtils::setComboValue(ui_->paymentFrequencyCombo, "");
    InstrumentFormUtils::setComboValue(ui_->returnTypeCombo, "");
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
