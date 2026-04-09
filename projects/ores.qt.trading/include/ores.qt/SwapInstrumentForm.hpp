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
#ifndef ORES_QT_SWAP_INSTRUMENT_FORM_HPP
#define ORES_QT_SWAP_INSTRUMENT_FORM_HPP

#include <chrono>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/IInstrumentForm.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"

namespace Ui {
class SwapInstrumentForm;
}

namespace ores::qt {

/**
 * @brief @c IInstrumentForm subclass owning the swap / rates instrument editor.
 *
 * Hosts an internal @c QTabWidget with two pages: core (always visible) and
 * rates extensions (revealed when @c has_extension is true on the trade type
 * row). Swap legs are loaded and saved through the form but are not editable
 * in the UI in this iteration.
 */
class SwapInstrumentForm final : public IInstrumentForm {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.swap_instrument_form";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Form-local state for the swap instrument editor.
     *
     * Aggregates fields from all nine rates instrument types into a single
     * flat struct so the form can display any type without storing a domain
     * object directly. Fields that are not applicable to the active trade type
     * will be empty/zero.
     *
     * This is NOT a domain type. It is never serialised or transmitted.
     */
    struct SwapFormState {
        // Identity
        boost::uuids::uuid instrument_id;
        std::optional<boost::uuids::uuid> trade_id;
        // Set by setTradeType(); not carried by any domain object.
        std::string trade_type_code;

        // Common to most types
        std::string start_date;
        std::string maturity_date; // FRA: maps from/to end_date
        std::string description;

        // FRA-specific
        std::string currency;
        double notional = 0.0;
        std::string rate_index;
        double strike = 0.0;
        std::string long_short;

        // BalanceGuaranteedSwap-specific
        std::optional<int> lockout_days;

        // Callable swap-specific
        std::string call_dates_json;
        std::string call_type;

        // RPA-specific
        std::string reference_counterparty;
        double participation_rate = 0.0;
        std::optional<double> protection_fee;

        // Inflation swap-specific
        std::string inflation_index_code;
        std::optional<double> base_cpi;
        std::string lag_convention;

        // Provenance
        int version = 0;
        std::string modified_by;
        std::string performed_by;
        std::chrono::system_clock::time_point recorded_at;
        std::string change_reason_code;
        std::string change_commentary;
    };

    explicit SwapInstrumentForm(QWidget* parent = nullptr);
    ~SwapInstrumentForm() override;

    void setClientManager(ClientManager* cm) override;
    void setUsername(const std::string& username) override;

    void loadInstrument(const std::string& id) override;
    void clear() override;

    void setTradeType(const QString& code,
        bool has_options, bool has_extension) override;

    void setReadOnly(bool readOnly) override;
    bool isDirty() const override;
    bool isLoaded() const override;

    void setChangeReason(
        const std::string& code, const std::string& commentary) override;
    void writeUiToInstrument() override;

    void saveInstrument(
        std::function<void(const std::string& id)> on_success,
        std::function<void(const QString& error)> on_failure) override;

private:
    void setupConnections();
    void populateFromState();
    void emitProvenance();
    void onFieldChanged();

    Ui::SwapInstrumentForm* ui_;
    ClientManager* clientManager_ = nullptr;
    std::string username_;
    SwapFormState state_;
    std::vector<trading::domain::swap_leg> legs_;
    bool dirty_ = false;
    bool loaded_ = false;
};

}

#endif
