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
#include "ores.qt/SwapInstrumentForm.hpp"

#include <QPointer>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include "ui_SwapInstrumentForm.h"
#include "ores.qt/ClientManager.hpp"
#include "ores.trading.api/messaging/instrument_protocol.hpp"

namespace ores::qt {

using namespace ores::logging;

SwapInstrumentForm::SwapInstrumentForm(QWidget* parent)
    : IInstrumentForm(parent),
      ui_(new Ui::SwapInstrumentForm) {
    ui_->setupUi(this);
    // Rates extension sub-tab is hidden until setTradeType() reveals it.
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->ratesTab), false);
    setupConnections();
}

SwapInstrumentForm::~SwapInstrumentForm() = default;

void SwapInstrumentForm::setupConnections() {
    auto markChanged = [this]() { onFieldChanged(); };
    connect(ui_->tradeTypeCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->currencyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->startDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->maturityDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->descriptionEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->fraFixingDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->fraSettlementDateEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->rpaCounterpartyEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->inflationIndexCodeEdit, &QLineEdit::textChanged, this, markChanged);
    connect(ui_->callableDatesJsonEdit, &QPlainTextEdit::textChanged, this, markChanged);
    connect(ui_->notionalSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->lockoutDaysSpinBox,
            QOverload<int>::of(&QSpinBox::valueChanged),
            this, markChanged);
    connect(ui_->baseCpiSpinBox,
            QOverload<double>::of(&QDoubleSpinBox::valueChanged),
            this, markChanged);
}

void SwapInstrumentForm::setClientManager(ClientManager* cm) {
    clientManager_ = cm;
}

void SwapInstrumentForm::setUsername(const std::string& username) {
    username_ = username;
}

void SwapInstrumentForm::clear() {
    instrument_ = trading::domain::instrument{};
    legs_.clear();
    loaded_ = false;
    dirty_ = false;
    populateFromInstrument();
}

void SwapInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool has_extension) {
    instrument_.trade_type_code = code.trimmed().toStdString();
    ui_->subTabWidget->setTabVisible(
        ui_->subTabWidget->indexOf(ui_->ratesTab), has_extension);
}

void SwapInstrumentForm::setReadOnly(bool readOnly) {
    ui_->tradeTypeCodeEdit->setReadOnly(readOnly);
    ui_->notionalSpinBox->setReadOnly(readOnly);
    ui_->currencyEdit->setReadOnly(readOnly);
    ui_->startDateEdit->setReadOnly(readOnly);
    ui_->maturityDateEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->fraFixingDateEdit->setReadOnly(readOnly);
    ui_->fraSettlementDateEdit->setReadOnly(readOnly);
    ui_->lockoutDaysSpinBox->setReadOnly(readOnly);
    ui_->callableDatesJsonEdit->setReadOnly(readOnly);
    ui_->rpaCounterpartyEdit->setReadOnly(readOnly);
    ui_->inflationIndexCodeEdit->setReadOnly(readOnly);
    ui_->baseCpiSpinBox->setReadOnly(readOnly);
}

bool SwapInstrumentForm::isDirty() const { return dirty_; }
bool SwapInstrumentForm::isLoaded() const { return loaded_; }

void SwapInstrumentForm::setChangeReason(
    const std::string& code, const std::string& commentary) {
    instrument_.change_reason_code = code;
    instrument_.change_commentary = commentary;
}

void SwapInstrumentForm::writeUiToInstrument() {
    instrument_.trade_type_code =
        ui_->tradeTypeCodeEdit->text().trimmed().toStdString();
    instrument_.notional = ui_->notionalSpinBox->value();
    instrument_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    instrument_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    instrument_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    instrument_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    instrument_.fra_fixing_date =
        ui_->fraFixingDateEdit->text().trimmed().toStdString();
    instrument_.fra_settlement_date =
        ui_->fraSettlementDateEdit->text().trimmed().toStdString();
    {
        const int ld = ui_->lockoutDaysSpinBox->value();
        instrument_.lockout_days = (ld > 0)
            ? std::optional<int>(ld) : std::nullopt;
    }
    instrument_.callable_dates_json =
        ui_->callableDatesJsonEdit->toPlainText().trimmed().toStdString();
    instrument_.rpa_counterparty =
        ui_->rpaCounterpartyEdit->text().trimmed().toStdString();
    instrument_.inflation_index_code =
        ui_->inflationIndexCodeEdit->text().trimmed().toStdString();
    {
        const double cpi = ui_->baseCpiSpinBox->value();
        instrument_.base_cpi = (cpi > 0.0)
            ? std::optional<double>(cpi) : std::nullopt;
    }
    instrument_.modified_by = username_;
    instrument_.performed_by = username_;
}

void SwapInstrumentForm::loadInstrument(const std::string& id) {
    if (!clientManager_) return;

    struct LoadResult {
        bool success;
        std::string message;
        trading::domain::instrument instrument;
        std::vector<trading::domain::swap_leg> legs;
    };

    QPointer<SwapInstrumentForm> self = this;
    auto* watcher = new QFutureWatcher<LoadResult>(self);
    connect(watcher, &QFutureWatcher<LoadResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();
        if (!self) return;

        if (!result.success) {
            BOOST_LOG_SEV(lg(), warn)
                << "Failed to load swap instrument: " << result.message;
            emit self->loadFailed(QString::fromStdString(result.message));
            return;
        }

        self->instrument_ = std::move(result.instrument);
        self->legs_ = std::move(result.legs);
        self->loaded_ = true;
        self->dirty_ = false;
        self->populateFromInstrument();
        self->emitProvenance();
        emit self->instrumentLoaded();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, id]() -> LoadResult {
        if (!cm)
            return {false, "Dialog closed", {}, {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = trading::domain::product_type::swap;
        req.instrument_id = id;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}, {}};
        if (!r->success)
            return {false, r->message, {}, {}};

        const auto* swap =
            std::get_if<trading::messaging::swap_export_result>(&r->instrument);
        if (!swap)
            return {false, "Unexpected instrument type in response", {}, {}};

        return {true, {}, swap->instrument, swap->legs};
    }));
}

void SwapInstrumentForm::populateFromInstrument() {
    const auto block = [this](bool b) {
        ui_->tradeTypeCodeEdit->blockSignals(b);
        ui_->notionalSpinBox->blockSignals(b);
        ui_->currencyEdit->blockSignals(b);
        ui_->startDateEdit->blockSignals(b);
        ui_->maturityDateEdit->blockSignals(b);
        ui_->descriptionEdit->blockSignals(b);
        ui_->fraFixingDateEdit->blockSignals(b);
        ui_->fraSettlementDateEdit->blockSignals(b);
        ui_->lockoutDaysSpinBox->blockSignals(b);
        ui_->callableDatesJsonEdit->blockSignals(b);
        ui_->rpaCounterpartyEdit->blockSignals(b);
        ui_->inflationIndexCodeEdit->blockSignals(b);
        ui_->baseCpiSpinBox->blockSignals(b);
    };

    block(true);
    ui_->tradeTypeCodeEdit->setText(
        QString::fromStdString(instrument_.trade_type_code));
    ui_->notionalSpinBox->setValue(instrument_.notional);
    ui_->currencyEdit->setText(
        QString::fromStdString(instrument_.currency));
    ui_->startDateEdit->setText(
        QString::fromStdString(instrument_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(instrument_.maturity_date));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(instrument_.description));
    ui_->fraFixingDateEdit->setText(
        QString::fromStdString(instrument_.fra_fixing_date));
    ui_->fraSettlementDateEdit->setText(
        QString::fromStdString(instrument_.fra_settlement_date));
    ui_->lockoutDaysSpinBox->setValue(
        instrument_.lockout_days.value_or(0));
    ui_->callableDatesJsonEdit->setPlainText(
        QString::fromStdString(instrument_.callable_dates_json));
    ui_->rpaCounterpartyEdit->setText(
        QString::fromStdString(instrument_.rpa_counterparty));
    ui_->inflationIndexCodeEdit->setText(
        QString::fromStdString(instrument_.inflation_index_code));
    ui_->baseCpiSpinBox->setValue(
        instrument_.base_cpi.value_or(0.0));
    block(false);
}

void SwapInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = instrument_.version;
    p.modified_by = instrument_.modified_by;
    p.performed_by = instrument_.performed_by;
    p.recorded_at = instrument_.recorded_at;
    p.change_reason_code = instrument_.change_reason_code;
    p.change_commentary = instrument_.change_commentary;
    emit provenanceChanged(p);
}

void SwapInstrumentForm::onFieldChanged() {
    if (!loaded_) return;
    dirty_ = true;
    emit changed();
}

void SwapInstrumentForm::saveInstrument(
    std::function<void(const std::string&)> on_success,
    std::function<void(const QString&)> on_failure) {

    if (!clientManager_) {
        on_failure(QStringLiteral("Dialog closed"));
        return;
    }

    struct SaveResult { bool success; std::string message; };

    QPointer<SwapInstrumentForm> self = this;
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
                << "Swap instrument save failed: " << result.message;
            on_failure(QString::fromStdString(result.message));
            return;
        }

        BOOST_LOG_SEV(lg(), info) << "Swap instrument saved";
        self->dirty_ = false;
        self->emitProvenance();
        on_success(boost::uuids::to_string(self->instrument_.id));
    });

    auto* cm = clientManager_;
    auto instrument = instrument_;
    auto legs = legs_;
    watcher->setFuture(QtConcurrent::run(
        [cm,
         instrument = std::move(instrument),
         legs = std::move(legs)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};
        trading::messaging::save_instrument_request req;
        req.data = instrument;
        req.legs = legs;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r) return {false, "Failed to communicate with server"};
        return {r->success, r->message};
    }));
}

}
