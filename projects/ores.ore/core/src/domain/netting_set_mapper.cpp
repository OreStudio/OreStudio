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
#include "ores.ore.core/domain/netting_set_mapper.hpp"
#include "ores.ore.core/domain/ore_code_tables.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include <algorithm>
#include <map>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>
#include <boost/functional/hash.hpp>

namespace ores::ore::domain {

using namespace ores::logging;

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

boost::uuids::uuid new_uuid() {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    return generator();
}

template <typename T>
void set_audit(T& r) {
    r.modified_by = std::string(audit_modified_by);
    r.performed_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);
}

template <typename T>
std::optional<std::string> text(const xsd::optional<T>& v) {
    if (!v)
        return std::nullopt;
    return std::string(*v);
}

template <typename T>
std::optional<double> number(const xsd::optional<T>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<double>(*v);
}

template <typename T>
std::optional<bool> flag(const xsd::optional<T>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<bool>(*v);
}

std::optional<std::string> csa_type_text(const xsd::optional<csaType>& v) {
    if (!v)
        return std::nullopt;
    return to_string(*v);
}

csaType parse_csa_type(const std::string& s) {
    static const std::map<std::string, csaType> types = {
        {"Bilateral", csaType::Bilateral},
        {"CallOnly", csaType::CallOnly},
        {"PostOnly", csaType::PostOnly}};
    if (auto it = types.find(s); it != types.end())
        return it->second;
    throw std::runtime_error("Invalid CSA type: " + s);
}

independentAmountType parse_independent_amount_type(const std::string& s) {
    if (s == "FIXED")
        return independentAmountType::FIXED;
    throw std::runtime_error("Invalid independent amount type: " + s);
}

template <typename T>
void put_text(xsd::optional<T>& target, const std::optional<std::string>& v) {
    if (!v)
        return;
    T value;
    static_cast<xsd::string&>(value) = *v;
    target = std::move(value);
}

template <typename T>
void put(xsd::optional<T>& target, const std::optional<T>& v) {
    if (v)
        target = *v;
}

refdata::domain::csa map_csa(const nettingsetdefinitions_NettingSet_t_CSADetails_t& d,
                             const boost::uuids::uuid& set_id, bool active) {
    refdata::domain::csa r;
    r.id = new_uuid();
    r.netting_set_id = set_id;
    r.is_active = active;
    r.bilateral = csa_type_text(d.Bilateral);
    if (d.CSACurrency)
        r.csa_currency = to_string(*d.CSACurrency);
    r.index_name = text(d.Index);
    r.threshold_pay = number(d.ThresholdPay);
    r.threshold_receive = number(d.ThresholdReceive);
    r.minimum_transfer_amount_pay = number(d.MinimumTransferAmountPay);
    r.minimum_transfer_amount_receive = number(d.MinimumTransferAmountReceive);
    if (d.IndependentAmount) {
        r.independent_amount_held = d.IndependentAmount->IndependentAmountHeld;
        r.independent_amount_type = to_string(d.IndependentAmount->IndependentAmountType);
    }
    if (d.MarginingFrequency) {
        r.call_frequency = std::string(d.MarginingFrequency->CallFrequency);
        r.post_frequency = std::string(d.MarginingFrequency->PostFrequency);
    }
    r.margin_period_of_risk = text(d.MarginPeriodOfRisk);
    r.collateral_compounding_spread_receive = number(d.CollateralCompoundingSpreadReceive);
    r.collateral_compounding_spread_pay = number(d.CollateralCompoundingSpreadPay);
    r.apply_initial_margin = flag(d.ApplyInitialMargin);
    r.initial_margin_type = csa_type_text(d.InitialMarginType);
    r.calculate_im_amount = flag(d.CalculateIMAmount);
    r.calculate_vm_amount = flag(d.CalculateVMAmount);
    r.non_exempt_im_regulations = text(d.NonExemptIMRegulations);
    set_audit(r);
    return r;
}

nettingsetdefinitions_NettingSet_t_CSADetails_t
reverse_csa(const refdata::domain::csa& c,
            const std::vector<const refdata::domain::csa_eligible_currency*>& currencies) {
    nettingsetdefinitions_NettingSet_t_CSADetails_t d;
    if (c.bilateral)
        d.Bilateral = parse_csa_type(*c.bilateral);
    if (c.csa_currency)
        d.CSACurrency = parse_currency_code(*c.csa_currency);
    put_text(d.Index, c.index_name);
    put(d.ThresholdPay, c.threshold_pay);
    put(d.ThresholdReceive, c.threshold_receive);
    put(d.MinimumTransferAmountPay, c.minimum_transfer_amount_pay);
    put(d.MinimumTransferAmountReceive, c.minimum_transfer_amount_receive);
    if (c.independent_amount_held.has_value() != c.independent_amount_type.has_value())
        throw std::runtime_error(
            "A CSA states half of its independent amount; ORE needs both the amount and its type.");
    if (c.independent_amount_held) {
        nettingsetdefinitions_NettingSet_t_CSADetails_t_IndependentAmount_t amount;
        amount.IndependentAmountHeld = *c.independent_amount_held;
        amount.IndependentAmountType = parse_independent_amount_type(*c.independent_amount_type);
        d.IndependentAmount = std::move(amount);
    }
    if (c.call_frequency.has_value() != c.post_frequency.has_value())
        throw std::runtime_error(
            "A CSA states one margining frequency; ORE needs both the call and the post frequency.");
    if (c.call_frequency) {
        nettingsetdefinitions_NettingSet_t_CSADetails_t_MarginingFrequency_t frequency;
        static_cast<xsd::string&>(frequency.CallFrequency) = *c.call_frequency;
        static_cast<xsd::string&>(frequency.PostFrequency) = *c.post_frequency;
        d.MarginingFrequency = std::move(frequency);
    }
    put_text(d.MarginPeriodOfRisk, c.margin_period_of_risk);
    put(d.CollateralCompoundingSpreadReceive, c.collateral_compounding_spread_receive);
    put(d.CollateralCompoundingSpreadPay, c.collateral_compounding_spread_pay);
    if (!currencies.empty()) {
        nettingsetdefinitions_NettingSet_t_CSADetails_t_EligibleCollaterals_t collaterals;
        for (const auto* currency : currencies)
            collaterals.Currencies.Currency.push_back(
                parse_currency_code(currency->currency_code));
        d.EligibleCollaterals = std::move(collaterals);
    }
    put(d.ApplyInitialMargin, c.apply_initial_margin);
    if (c.initial_margin_type)
        d.InitialMarginType = parse_csa_type(*c.initial_margin_type);
    put(d.CalculateIMAmount, c.calculate_im_amount);
    put(d.CalculateVMAmount, c.calculate_vm_amount);
    put_text(d.NonExemptIMRegulations, c.non_exempt_im_regulations);
    return d;
}

}

mapped_netting_sets netting_set_mapper::map(const nettingsetdefinitions& v) {
    BOOST_LOG_SEV(lg(), debug) << "Mapping " << v.NettingSet.size() << " ORE netting sets.";

    mapped_netting_sets r;
    for (const auto& n : v.NettingSet) {
        refdata::domain::netting_set s;
        s.id = new_uuid();
        const auto& group = n.nettingSetGroup;
        if (group.NettingSetDetails) {
            const auto& details = *group.NettingSetDetails;
            if (details.AgreementType || details.LegalEntityId)
                throw std::runtime_error(
                    "Netting set " + std::string(details.NettingSetId) +
                    " names an agreement type or a legal entity, which must resolve to "
                    "a netting agreement and a party before it can be imported.");
            s.code = std::string(details.NettingSetId);
            s.call_type = text(details.CallType);
            s.initial_margin_type = text(details.InitialMarginType);
        } else if (group.NettingSetId) {
            s.code = std::string(*group.NettingSetId);
        } else {
            throw std::runtime_error("A netting set has neither an id nor details.");
        }
        s.risk_weight = number(n.RiskWeight);
        set_audit(s);

        if (n.CSADetails) {
            const bool active = n.ActiveCSAFlag && static_cast<bool>(*n.ActiveCSAFlag);
            auto c = map_csa(*n.CSADetails, s.id, active);
            if (n.CSADetails->EligibleCollaterals) {
                int position = 0;
                for (const auto& code : n.CSADetails->EligibleCollaterals->Currencies.Currency) {
                    refdata::domain::csa_eligible_currency e;
                    e.id = new_uuid();
                    e.csa_id = c.id;
                    e.currency_code = to_string(code);
                    e.position = position++;
                    set_audit(e);
                    r.eligible_currencies.push_back(std::move(e));
                }
            }
            r.csas.push_back(std::move(c));
        } else if (n.ActiveCSAFlag && static_cast<bool>(*n.ActiveCSAFlag)) {
            throw std::runtime_error("Netting set " + s.code +
                                     " has an active CSA flag but no CSA details.");
        }
        r.sets.push_back(std::move(s));
    }
    return r;
}

nettingsetdefinitions netting_set_mapper::reverse(const mapped_netting_sets& v) {
    BOOST_LOG_SEV(lg(), debug) << "Reversing " << v.sets.size() << " netting sets.";

    using uuid_hash = boost::hash<boost::uuids::uuid>;
    std::unordered_map<boost::uuids::uuid, const refdata::domain::csa*, uuid_hash> csa_by_set;
    for (const auto& c : v.csas)
        csa_by_set.emplace(c.netting_set_id, &c);
    std::unordered_map<boost::uuids::uuid,
                       std::vector<const refdata::domain::csa_eligible_currency*>, uuid_hash>
        currencies_by_csa;
    for (const auto& e : v.eligible_currencies)
        currencies_by_csa[e.csa_id].push_back(&e);

    nettingsetdefinitions r;
    for (const auto& s : v.sets) {
        nettingsetdefinitions_NettingSet_t n;
        if (s.call_type || s.initial_margin_type) {
            nettingSetDetails details;
            static_cast<xsd::string&>(details.NettingSetId) = s.code;
            put_text(details.CallType, s.call_type);
            put_text(details.InitialMarginType, s.initial_margin_type);
            n.nettingSetGroup.NettingSetDetails = std::move(details);
        } else {
            _NettingSetId_t id;
            static_cast<xsd::string&>(id) = s.code;
            n.nettingSetGroup.NettingSetId = std::move(id);
        }

        const auto csa = csa_by_set.find(s.id);
        n.ActiveCSAFlag = csa != csa_by_set.end() && csa->second->is_active;
        if (csa != csa_by_set.end()) {
            auto currencies = currencies_by_csa[csa->second->id];
            std::ranges::sort(currencies, {}, &refdata::domain::csa_eligible_currency::position);
            n.CSADetails = reverse_csa(*csa->second, currencies);
        }
        put(n.RiskWeight, s.risk_weight);
        r.NettingSet.push_back(std::move(n));
    }
    return r;
}

}
