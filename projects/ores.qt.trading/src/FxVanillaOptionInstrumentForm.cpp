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
#include "ores.qt/FxVanillaOptionInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_FxVanillaOptionInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

FxVanillaOptionInstrumentForm::FxVanillaOptionInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::FxVanillaOptionInstrumentForm) {
    ui_->setupUi(this);
    setupConnections();
}

FxVanillaOptionInstrumentForm::~FxVanillaOptionInstrumentForm() = default;

void FxVanillaOptionInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->boughtCurrencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->soldCurrencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->expiryDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->exerciseStyleEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->settlementEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->boughtAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->soldAmountSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void FxVanillaOptionInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void FxVanillaOptionInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void FxVanillaOptionInstrumentForm::clear() {
    instrument_ = trading::domain::fx_vanilla_option_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void FxVanillaOptionInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool /*has_extension*/) {
    instrument_.trade_type_code = code.trimmed().toStdString();
}

void FxVanillaOptionInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->boughtCurrencyEdit->setReadOnly(readOnly);
    ui_->boughtAmountSpinBox->setReadOnly(readOnly);
    ui_->soldCurrencyEdit->setReadOnly(readOnly);
    ui_->soldAmountSpinBox->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->expiryDateEdit->setReadOnly(readOnly);
    ui_->exerciseStyleEdit->setReadOnly(readOnly);
    ui_->settlementEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
}

bool FxVanillaOptionInstrumentForm::isDirty() const { return dirty_; }
bool FxVanillaOptionInstrumentForm::isLoaded() const { return loaded_; }

void FxVanillaOptionInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void FxVanillaOptionInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.bought_currency =
        ui_->boughtCurrencyEdit->text().trimmed().toStdString();
    instrument_.bought_amount = ui_->boughtAmountSpinBox->value();
    instrument_.sold_currency =
        ui_->soldCurrencyEdit->text().trimmed().toStdString();
    instrument_.sold_amount = ui_->soldAmountSpinBox->value();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.expiry_date =
        ui_->expiryDateEdit->text().trimmed().toStdString();
    instrument_.exercise_style =
        ui_->exerciseStyleEdit->text().trimmed().toStdString();
    instrument_.settlement =
        ui_->settlementEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void FxVanillaOptionInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* ex =
        std::get_if<trading::messaging::fx_export_result>(&instrument);
    if (!ex) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-FX instrument pushed to FxVanillaOptionInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for FX vanilla option form"));
        return;
    }
    const auto* opt =
        std::get_if<trading::domain::fx_vanilla_option_instrument>(&ex->instrument);
    if (!opt) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-vanilla-option FX type pushed to FxVanillaOptionInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected FX sub-type for vanilla option form"));
        return;
    }

    instrument_ = *opt;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void FxVanillaOptionInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->boughtCurrencyEdit->blockSignals(b);
        ui_->boughtAmountSpinBox->blockSignals(b);
        ui_->soldCurrencyEdit->blockSignals(b);
        ui_->soldAmountSpinBox->blockSignals(b);
        ui_->optionTypeEdit->blockSignals(b);
        ui_->expiryDateEdit->blockSignals(b);
        ui_->exerciseStyleEdit->blockSignals(b);
        ui_->settlementEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->boughtCurrencyEdit->setText(
        QString::fromStdString(instrument_.bought_currency));
    ui_->boughtAmountSpinBox->setValue(instrument_.bought_amount);
    ui_->soldCurrencyEdit->setText(
        QString::fromStdString(instrument_.sold_currency));
    ui_->soldAmountSpinBox->setValue(instrument_.sold_amount);
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->expiryDateEdit->setText(
        QString::fromStdString(instrument_.expiry_date));
    ui_->exerciseStyleEdit->setText(
        QString::fromStdString(instrument_.exercise_style));
    ui_->settlementEdit->setText(
        QString::fromStdString(instrument_.settlement));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    block(false);
}

void FxVanillaOptionInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void FxVanillaOptionInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void FxVanillaOptionInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<FxVanillaOptionInstrumentForm> self = this;
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
                << "FX vanilla option save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "FX vanilla option instrument saved";
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
        trading::messaging::save_fx_vanilla_option_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
