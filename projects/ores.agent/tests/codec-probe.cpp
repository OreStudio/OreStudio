// SPDX-License-Identifier: GPL-3.0-or-later
// Test helper only. Compile against the candidate checkout's API headers.
#include "ores.refdata.api/domain/country_codec.hpp"
#include "ores.refdata.api/domain/currency_country_codec.hpp"
#include "ores.reporting.api/domain/configuration_analytic_codec.hpp"
#include "ores.reporting.api/domain/configuration_market_binding_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_building_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_execution_codec.hpp"
#include "ores.reporting.api/domain/engine_settings_fixings_codec.hpp"
#include "ores.reporting.api/domain/report_configuration_codec.hpp"
#include "ores.reporting.api/domain/risk_report_config_codec.hpp"
#include "ores.trading.api/domain/trade_identifier_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <iostream>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>

#define AGENT_OPERATION(name, type)
#include "agent-contract-operations.inc"
#undef AGENT_OPERATION

std::string unhex(std::string_view text) {
    if (text.size() % 2 != 0)
        throw std::invalid_argument("Odd hex length");
    std::string bytes;
    for (std::size_t i = 0; i < text.size(); i += 2)
        bytes.push_back(static_cast<char>(std::stoul(std::string(text.substr(i, 2)), nullptr, 16)));
    return bytes;
}

std::string hex(std::string_view bytes) {
    constexpr char digits[] = "0123456789abcdef";
    std::string text;
    for (unsigned char c : bytes) {
        text.push_back(digits[c >> 4]);
        text.push_back(digits[c & 15]);
    }
    return text;
}

template <typename Request>
void probe(std::string_view bytes, std::string_view response_json) {
    using namespace ores::utility::codec;
    // Strict server request reader: missing required fields and wrong types fail.
    auto request =
        object::read_msgpack<Request>(std::as_bytes(std::span(bytes.data(), bytes.size())));
    // Test data can omit unused response fields. The current server supplies their defaults.
    auto response = json::read_lenient<typename Request::response_type>(response_json);
    if (!response)
        throw std::runtime_error(response.error());
    std::cout << "{\"sourceSha256\":\"" << AGENT_CONTRACT_SOURCE_SHA << "\",\"subject\":\""
              << Request::nats_subject << "\",\"request\":" << object::write_json(request)
              << ",\"responseHex\":\"" << hex(object::write_msgpack(*response)) << "\"}\n";
}

int main(int argc, char** argv) {
    try {
        if (argc != 4)
            throw std::invalid_argument("Pass operation, request hex and response JSON");
        const auto bytes = unhex(argv[2]);
#define AGENT_OPERATION(name, type)          \
    if (std::string_view(argv[1]) == name) { \
        probe<type>(bytes, argv[3]);         \
        return 0;                            \
    }
#include "agent-contract-operations.inc"
#undef AGENT_OPERATION
        throw std::invalid_argument("Unknown operation");
    } catch (const std::exception& error) {
        std::cerr << error.what() << '\n';
        return 1;
    }
}
