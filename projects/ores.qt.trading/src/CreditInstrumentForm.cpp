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
#include "ores.qt/CreditInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_CreditInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

CreditInstrumentForm::CreditInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::CreditInstrumentForm) {
    ui_->setupUi(this);
    // Extensions tab hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), false);
    setupConnections();
}

CreditInstrumentForm::~CreditInstrumentForm() = default;

void CreditInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->referenceEntityEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->tenorEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->dayCountCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->paymentFrequencyCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->indexNameEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->seniorityEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->restructuringEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->optionTypeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->optionExpiryDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->linkedAssetCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->spreadSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->recoveryRateSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->indexSeriesSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->optionStrikeSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->trancheAttachmentSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->trancheDetachmentSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void CreditInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void CreditInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void CreditInstrumentForm::clear() {
    instrument_ = trading::domain::credit_instrument{};
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void CreditInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool has_extension) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->extensionsTab), has_extension);
}

void CreditInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->referenceEntityEdit->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->spreadSpinBox->setReadOnly(readOnly);
    ui_->recoveryRateSpinBox->setReadOnly(readOnly);
    ui_->tenorEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->dayCountCodeEdit->setReadOnly(readOnly);
    ui_->paymentFrequencyCodeEdit->setReadOnly(readOnly);
    ui_->indexNameEdit->setReadOnly(readOnly);
    ui_->indexSeriesSpinBox->setReadOnly(readOnly);
    ui_->seniorityEdit->setReadOnly(readOnly);
    ui_->restructuringEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->optionTypeEdit->setReadOnly(readOnly);
    ui_->optionExpiryDateEdit->setReadOnly(readOnly);
    ui_->optionStrikeSpinBox->setReadOnly(readOnly);
    ui_->linkedAssetCodeEdit->setReadOnly(readOnly);
    ui_->trancheAttachmentSpinBox->setReadOnly(readOnly);
    ui_->trancheDetachmentSpinBox->setReadOnly(readOnly);
}

bool CreditInstrumentForm::isDirty() const { return dirty_; }
bool CreditInstrumentForm::isLoaded() const { return loaded_; }

void CreditInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void CreditInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.reference_entity =
        ui_->referenceEntityEdit->text().trimmed().toStdString();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.spread = ui_->spreadSpinBox->value();
    instrument_.recovery_rate = ui_->recoveryRateSpinBox->value();
    instrument_.tenor =
        ui_->tenorEdit->text().trimmed().toStdString();
    instrument_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.day_count_code =
        ui_->dayCountCodeEdit->text().trimmed().toStdString();
    instrument_.payment_frequency_code =
        ui_->paymentFrequencyCodeEdit->text().trimmed().toStdString();
    instrument_.index_name =
        ui_->indexNameEdit->text().trimmed().toStdString();
    instrument_.index_series = ui_->indexSeriesSpinBox->value();
    instrument_.seniority =
        ui_->seniorityEdit->text().trimmed().toStdString();
    instrument_.restructuring =
        ui_->restructuringEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.option_type =
        ui_->optionTypeEdit->text().trimmed().toStdString();
    instrument_.option_expiry_date =
        ui_->optionExpiryDateEdit->text().trimmed().toStdString();
    {
        const double s = ui_->optionStrikeSpinBox->value();
        instrument_.option_strike = (s > 0.0)
            ? std::optional<double>(s) : std::nullopt;
    }
    instrument_.linked_asset_code =
        ui_->linkedAssetCodeEdit->text().trimmed().toStdString();
    {
        const double a = ui_->trancheAttachmentSpinBox->value();
        instrument_.tranche_attachment = (a > 0.0)
            ? std::optional<double>(a) : std::nullopt;
    }
    {
        const double d = ui_->trancheDetachmentSpinBox->value();
        instrument_.tranche_detachment = (d > 0.0)
            ? std::optional<double>(d) : std::nullopt;
    }
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void CreditInstrumentForm::setInstrument(
    const trading::messaging::instrument_export_result& instrument) {

    const auto* credit =
        std::get_if<trading::domain::credit_instrument>(&instrument);
    if (!credit) {
        BOOST_LOG_SEV(lg(), warn)
            << "Non-credit instrument pushed to CreditInstrumentForm";
        emit loadFailed(QStringLiteral(
            "Unexpected instrument type for credit form"));
        return;
    }

    instrument_ = *credit;
    loaded_ = true;
    dirty_ = false;
    populateFromInstrument();
    emitProvenance();
    emit instrumentLoaded();
}

void CreditInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->referenceEntityEdit->blockSignals(b);
        ui_->currencyEdit->blockSignals(b);
        ui_->notionalSpinBox->blockSignals(b);
        ui_->spreadSpinBox->blockSignals(b);
        ui_->recoveryRateSpinBox->blockSignals(b);
        ui_->tenorEdit->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->dayCountCodeEdit->blockSignals(b);
        ui_->paymentFrequencyCodeEdit->blockSignals(b);
        ui_->indexNameEdit->blockSignals(b);
        ui_->indexSeriesSpinBox->blockSignals(b);
        ui_->seniorityEdit->blockSignals(b);
        ui_->restructuringEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->optionTypeEdit->blockSignals(b);
        ui_->optionExpiryDateEdit->blockSignals(b);
        ui_->optionStrikeSpinBox->blockSignals(b);
        ui_->linkedAssetCodeEdit->blockSignals(b);
        ui_->trancheAttachmentSpinBox->blockSignals(b);
        ui_->trancheDetachmentSpinBox->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->referenceEntityEdit->setText(
        QString::fromStdString(instrument_.reference_entity));
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->spreadSpinBox->setValue(instrument_.spread);
    ui_->recoveryRateSpinBox->setValue(instrument_.recovery_rate);
    ui_->tenorEdit->setText(
        QString::fromStdString(instrument_.tenor));
    ui_->startDateEdit->setText(
        QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->dayCountCodeEdit->setText(
        QString::fromStdString(instrument_.day_count_code));
    ui_->paymentFrequencyCodeEdit->setText(
        QString::fromStdString(instrument_.payment_frequency_code));
    ui_->indexNameEdit->setText(
        QString::fromStdString(instrument_.index_name));
    ui_->indexSeriesSpinBox->setValue(instrument_.index_series);
    ui_->seniorityEdit->setText(
        QString::fromStdString(instrument_.seniority));
    ui_->restructuringEdit->setText(
        QString::fromStdString(instrument_.restructuring));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->optionTypeEdit->setText(
        QString::fromStdString(instrument_.option_type));
    ui_->optionExpiryDateEdit->setText(
        QString::fromStdString(instrument_.option_expiry_date));
    ui_->optionStrikeSpinBox->setValue(
        instrument_.option_strike.value_or(0.0));
    ui_->linkedAssetCodeEdit->setText(
        QString::fromStdString(instrument_.linked_asset_code));
    ui_->trancheAttachmentSpinBox->setValue(
        instrument_.tranche_attachment.value_or(0.0));
    ui_->trancheDetachmentSpinBox->setValue(
        instrument_.tranche_detachment.value_or(0.0));
    block(false);
}

void CreditInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void CreditInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void CreditInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<CreditInstrumentForm> self = this;
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
                << "Credit instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Credit instrument saved";
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
        trading::messaging::save_credit_instrument_request req;
        req.data = instrument;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
