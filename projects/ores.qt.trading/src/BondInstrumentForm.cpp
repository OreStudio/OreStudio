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

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_BondInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

BondInstrumentForm::BondInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::BondInstrumentForm) {
    ui_->setupUi(this);
    // Extensions tab is hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), false);
    setupConnections();
}

BondInstrumentForm::~BondInstrumentForm() = default;

void BondInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->issuerEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->issueDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->couponFrequencyCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->dayCountCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->callDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->futureExpiryDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->optionExpiryDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->trsReturnTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->trsFundingLegCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->ascotOptionTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->faceValueSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->couponRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->settlementDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->conversionRatioSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->optionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void BondInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
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
    bool /*has_options*/, bool has_extension) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void BondInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->issuerEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->faceValueSpinBox->setReadOnly(readOnly);
    ui_->couponRateSpinBox->setReadOnly(readOnly);
    ui_->couponFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->dayCountCodeEdit->setReadOnly(readOnly);
    ui_->issueDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->settlementDaysSpinBox->setReadOnly(readOnly);
    ui_->callDateEdit->setReadOnly(readOnly);
    ui_->conversionRatioSpinBox->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->futureExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->optionExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionStrikeSpinBox->setReadOnly(readOnly);
    ui_->trsReturnTypeEdit->setReadOnly(readOnly);
    ui_->trsFundingLegCodeEdit->setReadOnly(readOnly);
    ui_->ascotOptionTypeEdit->setReadOnly(readOnly);
}

bool BondInstrumentForm::isDirty() const { return dirty_; }
bool BondInstrumentForm::isLoaded() const { return loaded_; }

void BondInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void BondInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.issuer =
        ui_->issuerEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.face_value = ui_->faceValueSpinBox->value();
    instrument_.coupon_rate = ui_->couponRateSpinBox->value();
    instrument_.coupon_frequency_code =
        ui_->couponFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.day_count_code =
        ui_->dayCountCodeEdit->text().trimmed().toStdString();
    instrument_.issue_date =
        ui_->issueDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.settlement_days = ui_->settlementDaysSpinBox->value();
    instrument_.call_date =
        ui_->callDateEdit->text().trimmed().toStdString();
    instrument_.conversion_ratio = ui_->conversionRatioSpinBox->value();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.future_expiry_date =
        ui_->futureExpiryDateEdit->text().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.option_expiry_date =
        ui_->optionExpiryDateEdit->text().trimmed().toStdString();
    {
        const double s = ui_->optionStrikeSpinBox->value();
        instrument_.option_strike = (s > 0.0)
            ? std::optional<double>(s) : std::nullopt;
    }
    instrument_.trs_return_type =
        ui_->trsReturnTypeEdit->text().trimmed().toStdString();
    instrument_.trs_funding_leg_code =
        ui_->trsFundingLegCodeEdit->text().trimmed().toStdString();
    instrument_.ascot_option_type =
        ui_->ascotOptionTypeEdit->text().trimmed().toStdString();
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void BondInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* ex =
        std::get_if<trading::messaging::bond_export_result>(&instrument);
    if (!ex) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-bond instrument pushed to BondInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for bond form"));
        return;
    }

    instrument_ = ex->instrument;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void BondInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->issuerEdit->blockSignals(b);
        ui_->currencyEdit->blockSignals(b);
        ui_->faceValueSpinBox->blockSignals(b);
        ui_->couponRateSpinBox->blockSignals(b);
        ui_->couponFrequencyCodeEdit->blockSignals(b);
        ui_->dayCountCodeEdit->blockSignals(b);
        ui_->issueDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->settlementDaysSpinBox->blockSignals(b);
        ui_->callDateEdit->blockSignals(b);
        ui_->conversionRatioSpinBox->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->futureExpiryDateEdit->blockSignals(b);
        ui_->optionTypeEdit->blockSignals(b);
        ui_->optionExpiryDateEdit->blockSignals(b);
        ui_->optionStrikeSpinBox->blockSignals(b);
        ui_->trsReturnTypeEdit->blockSignals(b);
        ui_->trsFundingLegCodeEdit->blockSignals(b);
        ui_->ascotOptionTypeEdit->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->issuerEdit->setText(
        QString::fromStdString(instrument_.issuer));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->faceValueSpinBox->setValue(instrument_.face_value);
    ui_->couponRateSpinBox->setValue(instrument_.coupon_rate);
    ui_->couponFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.coupon_frequency_code));
    ui_->dayCountCodeEdit->setText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->issueDateEdit->setText(
        QString::fromStdString(instrument_.issue_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->settlementDaysSpinBox->setValue(instrument_.settlement_days);
    ui_->callDateEdit->setText(
        QString::fromStdString(instrument_.call_date));
    ui_->conversionRatioSpinBox->setValue(instrument_.conversion_ratio);
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->futureExpiryDateEdit->setText(
        QString::fromStdString(instrument_.future_expiry_date));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->optionExpiryDateEdit->setText(
        QString::fromStdString(instrument_.option_expiry_date));
    ui_->optionStrikeSpinBox->setValue(
        instrument_.option_strike.value_or(0.0));
    ui_->trsReturnTypeEdit->setText(
        QString::fromStdString(instrument_.trs_return_type));
    ui_->trsFundingLegCodeEdit->setText(
        QString::fromStdString(instrument_.trs_funding_leg_code));
    ui_->ascotOptionTypeEdit->setText(
        QString::fromStdString(instrument_.ascot_option_type));
    block(false);
}

void BondInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void BondInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void BondInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<BondInstrumentForm> self = this;
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
                << "Bond instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Bond instrument saved";
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
        trading::messaging::save_bond_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
