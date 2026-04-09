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
#include "ores.trading.api/domain/fra_instrument.hpp"
#include "ores.trading.api/domain/vanilla_swap_instrument.hpp"
#include "ores.trading.api/domain/cap_floor_instrument.hpp"
#include "ores.trading.api/domain/swaption_instrument.hpp"
#include "ores.trading.api/domain/balance_guaranteed_swap_instrument.hpp"
#include "ores.trading.api/domain/callable_swap_instrument.hpp"
#include "ores.trading.api/domain/knock_out_swap_instrument.hpp"
#include "ores.trading.api/domain/inflation_swap_instrument.hpp"
#include "ores.trading.api/domain/rpa_instrument.hpp"

namespace ores::qt {

using namespace ores::logging;

// ---------------------------------------------------------------------------
// State extractors: per-type domain → SwapFormState
// ---------------------------------------------------------------------------

namespace {

using State = SwapInstrumentForm::SwapFormState;

// Common provenance fields shared by all per-type domain structs.
template<typename T>
void copy_provenance(State& s, const T& i) {
    s.instrument_id = i.instrument_id;
    s.trade_id = i.trade_id;
    s.version = i.version;
    s.modified_by = i.modified_by;
    s.performed_by = i.performed_by;
    s.recorded_at = i.recorded_at;
    s.change_reason_code = i.change_reason_code;
    s.change_commentary = i.change_commentary;
}

State from_fra(const trading::domain::fra_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.end_date; // FRA uses end_date
    s.currency = i.currency;
    s.notional = i.notional;
    s.rate_index = i.rate_index;
    s.long_short = i.long_short;
    s.strike = i.strike;
    s.description = i.description;
    return s;
}

State from_vanilla_swap(const trading::domain::vanilla_swap_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.description = i.description;
    return s;
}

State from_cap_floor(const trading::domain::cap_floor_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.description = i.description;
    return s;
}

State from_swaption(const trading::domain::swaption_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.description = i.description;
    return s;
}

State from_balance_guaranteed_swap(
    const trading::domain::balance_guaranteed_swap_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.lockout_days = i.lockout_days;
    s.description = i.description;
    return s;
}

State from_callable_swap(const trading::domain::callable_swap_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.call_dates_json = i.call_dates_json;
    s.call_type = i.call_type;
    s.description = i.description;
    return s;
}

State from_knock_out_swap(
    const trading::domain::knock_out_swap_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.description = i.description;
    return s;
}

State from_inflation_swap(
    const trading::domain::inflation_swap_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.inflation_index_code = i.inflation_index_code;
    s.base_cpi = i.base_cpi;
    s.lag_convention = i.lag_convention;
    s.description = i.description;
    return s;
}

State from_rpa(const trading::domain::rpa_instrument& i) {
    State s;
    copy_provenance(s, i);
    s.start_date = i.start_date;
    s.maturity_date = i.maturity_date;
    s.reference_counterparty = i.reference_counterparty;
    s.participation_rate = i.participation_rate;
    s.protection_fee = i.protection_fee;
    s.description = i.description;
    return s;
}

State extract_state(const trading::messaging::swap_export_result& swap) {
    return std::visit([](const auto& instr) -> State {
        using T = std::decay_t<decltype(instr)>;
        using namespace trading::domain;
        if constexpr (std::is_same_v<T, fra_instrument>)
            return from_fra(instr);
        else if constexpr (std::is_same_v<T, vanilla_swap_instrument>)
            return from_vanilla_swap(instr);
        else if constexpr (std::is_same_v<T, cap_floor_instrument>)
            return from_cap_floor(instr);
        else if constexpr (std::is_same_v<T, swaption_instrument>)
            return from_swaption(instr);
        else if constexpr (std::is_same_v<T, balance_guaranteed_swap_instrument>)
            return from_balance_guaranteed_swap(instr);
        else if constexpr (std::is_same_v<T, callable_swap_instrument>)
            return from_callable_swap(instr);
        else if constexpr (std::is_same_v<T, knock_out_swap_instrument>)
            return from_knock_out_swap(instr);
        else if constexpr (std::is_same_v<T, inflation_swap_instrument>)
            return from_inflation_swap(instr);
        else // rpa_instrument
            return from_rpa(instr);
    }, swap.instrument);
}

// ---------------------------------------------------------------------------
// Domain builders: SwapFormState → per-type domain object
// ---------------------------------------------------------------------------

template<typename T>
void apply_provenance(T& instr, const State& s, const std::string& username) {
    instr.instrument_id = s.instrument_id;
    instr.trade_id = s.trade_id;
    instr.version = s.version;
    instr.modified_by = username;
    instr.performed_by = username;
    instr.change_reason_code = s.change_reason_code;
    instr.change_commentary = s.change_commentary;
}

trading::domain::fra_instrument build_fra(const State& s,
    const std::string& username) {
    trading::domain::fra_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.end_date = s.maturity_date; // FRA uses end_date
    r.currency = s.currency;
    r.notional = s.notional;
    r.rate_index = s.rate_index;
    r.long_short = s.long_short;
    r.strike = s.strike;
    r.description = s.description;
    return r;
}

trading::domain::vanilla_swap_instrument build_vanilla_swap(const State& s,
    const std::string& username) {
    trading::domain::vanilla_swap_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.description = s.description;
    return r;
}

trading::domain::cap_floor_instrument build_cap_floor(const State& s,
    const std::string& username) {
    trading::domain::cap_floor_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.description = s.description;
    return r;
}

trading::domain::swaption_instrument build_swaption(const State& s,
    const std::string& username) {
    trading::domain::swaption_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.description = s.description;
    return r;
}

trading::domain::balance_guaranteed_swap_instrument
build_balance_guaranteed_swap(const State& s, const std::string& username) {
    trading::domain::balance_guaranteed_swap_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.lockout_days = s.lockout_days;
    r.description = s.description;
    return r;
}

trading::domain::callable_swap_instrument build_callable_swap(const State& s,
    const std::string& username) {
    trading::domain::callable_swap_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.call_dates_json = s.call_dates_json;
    r.call_type = s.call_type;
    r.description = s.description;
    return r;
}

trading::domain::knock_out_swap_instrument build_knock_out_swap(const State& s,
    const std::string& username) {
    trading::domain::knock_out_swap_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.description = s.description;
    return r;
}

trading::domain::inflation_swap_instrument build_inflation_swap(const State& s,
    const std::string& username) {
    trading::domain::inflation_swap_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.inflation_index_code = s.inflation_index_code;
    r.base_cpi = s.base_cpi;
    r.lag_convention = s.lag_convention;
    r.description = s.description;
    return r;
}

trading::domain::rpa_instrument build_rpa(const State& s,
    const std::string& username) {
    trading::domain::rpa_instrument r;
    apply_provenance(r, s, username);
    r.start_date = s.start_date;
    r.maturity_date = s.maturity_date;
    r.reference_counterparty = s.reference_counterparty;
    r.participation_rate = s.participation_rate;
    r.protection_fee = s.protection_fee;
    r.description = s.description;
    return r;
}

} // namespace

// ---------------------------------------------------------------------------
// SwapInstrumentForm implementation
// ---------------------------------------------------------------------------

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
    const auto ttc = state_.trade_type_code;
    state_ = SwapFormState{};
    state_.trade_type_code = ttc;
    legs_.clear();
    loaded_ = false;
    dirty_ = false;
    populateFromState();
}

void SwapInstrumentForm::setTradeType(const QString& code,
    bool /*has_options*/, bool has_extension) {
    state_.trade_type_code = code.trimmed().toStdString();
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
    state_.change_reason_code = code;
    state_.change_commentary = commentary;
}

void SwapInstrumentForm::writeUiToInstrument() {
    state_.start_date =
        ui_->startDateEdit->text().trimmed().toStdString();
    state_.maturity_date =
        ui_->maturityDateEdit->text().trimmed().toStdString();
    state_.description =
        ui_->descriptionEdit->toPlainText().trimmed().toStdString();

    // FRA-specific
    state_.currency =
        ui_->currencyEdit->text().trimmed().toStdString();
    state_.notional = ui_->notionalSpinBox->value();

    // Callable swap-specific
    state_.call_dates_json =
        ui_->callableDatesJsonEdit->toPlainText().trimmed().toStdString();

    // RPA-specific
    state_.reference_counterparty =
        ui_->rpaCounterpartyEdit->text().trimmed().toStdString();

    // Inflation swap-specific
    state_.inflation_index_code =
        ui_->inflationIndexCodeEdit->text().trimmed().toStdString();
    {
        const double cpi = ui_->baseCpiSpinBox->value();
        state_.base_cpi = (cpi > 0.0) ? std::optional<double>(cpi) : std::nullopt;
    }

    state_.modified_by = username_;
    state_.performed_by = username_;
}

void SwapInstrumentForm::loadInstrument(const std::string& id) {
    if (!clientManager_) return;

    struct LoadResult {
        bool success;
        std::string message;
        SwapFormState state;
        std::vector<trading::domain::swap_leg> legs;
    };

    // Capture trade_type_code before launching async work.
    const std::string ttc = state_.trade_type_code;

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

        self->state_ = std::move(result.state);
        self->legs_ = std::move(result.legs);
        self->loaded_ = true;
        self->dirty_ = false;
        self->populateFromState();
        self->emitProvenance();
        emit self->instrumentLoaded();
    });

    auto* cm = clientManager_;
    watcher->setFuture(QtConcurrent::run([cm, id, ttc]() -> LoadResult {
        if (!cm)
            return {false, "Dialog closed", {}, {}};

        trading::messaging::get_instrument_for_trade_request req;
        req.product_type = trading::domain::product_type::swap;
        req.instrument_id = id;
        req.trade_type_code = ttc;
        auto r = cm->process_authenticated_request(std::move(req));
        if (!r)
            return {false, "Failed to communicate with server", {}, {}};
        if (!r->success)
            return {false, r->message, {}, {}};

        const auto* swap =
            std::get_if<trading::messaging::swap_export_result>(&r->instrument);
        if (!swap)
            return {false, "Unexpected instrument type in response", {}, {}};

        auto state = extract_state(*swap);
        state.trade_type_code = ttc; // not carried by domain objects
        return {true, {}, std::move(state), swap->legs};
    }));
}

void SwapInstrumentForm::populateFromState() {
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
        QString::fromStdString(state_.trade_type_code));
    ui_->notionalSpinBox->setValue(state_.notional);
    ui_->currencyEdit->setText(
        QString::fromStdString(state_.currency));
    ui_->startDateEdit->setText(
        QString::fromStdString(state_.start_date));
    ui_->maturityDateEdit->setText(
        QString::fromStdString(state_.maturity_date));
    ui_->descriptionEdit->setPlainText(
        QString::fromStdString(state_.description));
    // FRA extension widgets — not populated from per-type domain objects
    // (these UI widgets will be repurposed in a future UI redesign).
    ui_->fraFixingDateEdit->clear();
    ui_->fraSettlementDateEdit->clear();
    ui_->lockoutDaysSpinBox->setValue(state_.lockout_days.value_or(0));
    ui_->callableDatesJsonEdit->setPlainText(
        QString::fromStdString(state_.call_dates_json));
    ui_->rpaCounterpartyEdit->setText(
        QString::fromStdString(state_.reference_counterparty));
    ui_->inflationIndexCodeEdit->setText(
        QString::fromStdString(state_.inflation_index_code));
    ui_->baseCpiSpinBox->setValue(state_.base_cpi.value_or(0.0));
    block(false);
}

void SwapInstrumentForm::emitProvenance() {
    InstrumentProvenance p;
    p.version = state_.version;
    p.modified_by = state_.modified_by;
    p.performed_by = state_.performed_by;
    p.recorded_at = state_.recorded_at;
    p.change_reason_code = state_.change_reason_code;
    p.change_commentary = state_.change_commentary;
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
        on_success(boost::uuids::to_string(self->state_.instrument_id));
    });

    auto* cm = clientManager_;
    auto state = state_;
    auto username = username_;
    watcher->setFuture(QtConcurrent::run(
        [cm, state = std::move(state),
         username = std::move(username)]() -> SaveResult {
        if (!cm)
            return {false, "Dialog closed"};

        const auto& ttc = state.trade_type_code;
        auto send = [&](auto req) -> SaveResult {
            auto r = cm->process_authenticated_request(std::move(req));
            if (!r) return {false, "Failed to communicate with server"};
            return {r->success, r->message};
        };

        using namespace trading::messaging;
        if (ttc == "ForwardRateAgreement") {
            save_fra_instrument_request req;
            req.data = build_fra(state, username);
            return send(std::move(req));
        } else if (ttc == "Swap" || ttc == "CrossCurrencySwap"
                   || ttc == "FlexiSwap") {
            save_vanilla_swap_instrument_request req;
            req.data = build_vanilla_swap(state, username);
            return send(std::move(req));
        } else if (ttc == "CapFloor") {
            save_cap_floor_instrument_request req;
            req.data = build_cap_floor(state, username);
            return send(std::move(req));
        } else if (ttc == "Swaption") {
            save_swaption_instrument_request req;
            req.data = build_swaption(state, username);
            return send(std::move(req));
        } else if (ttc == "BalanceGuaranteedSwap") {
            save_balance_guaranteed_swap_instrument_request req;
            req.data = build_balance_guaranteed_swap(state, username);
            return send(std::move(req));
        } else if (ttc == "CallableSwap") {
            save_callable_swap_instrument_request req;
            req.data = build_callable_swap(state, username);
            return send(std::move(req));
        } else if (ttc == "KnockOutSwap") {
            save_knock_out_swap_instrument_request req;
            req.data = build_knock_out_swap(state, username);
            return send(std::move(req));
        } else if (ttc == "InflationSwap") {
            save_inflation_swap_instrument_request req;
            req.data = build_inflation_swap(state, username);
            return send(std::move(req));
        } else if (ttc == "RiskParticipationAgreement") {
            save_rpa_instrument_request req;
            req.data = build_rpa(state, username);
            return send(std::move(req));
        }
        return {false, "Unknown swap trade type: " + ttc};
    }));
}

}
