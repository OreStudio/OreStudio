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
#ifndef ORES_QT_COMMODITY_INSTRUMENT_FORM_HPP
#define ORES_QT_COMMODITY_INSTRUMENT_FORM_HPP

#include "ores.qt/IInstrumentForm.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"

namespace Ui {
class CommodityInstrumentForm;
}

namespace ores::qt {

/**
 * @brief @c IInstrumentForm subclass owning the commodity instrument editor.
 *
 * Hosts an internal @c QTabWidget with two pages: core (always visible)
 * and extensions (revealed when @c has_extension is true, e.g.
 * CommodityOption, CommoditySpreadOption, CommoditySwaption,
 * CommodityVarianceSwap, CommodityAccumulator).
 */
class CommodityInstrumentForm final : public IInstrumentForm {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.commodity_instrument_form";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CommodityInstrumentForm(QWidget* parent = nullptr);
    ~CommodityInstrumentForm() override;

    void setClientManager(ClientManager* cm) override;
    void setUsername(const std::string& username) override;

    void setInstrument(
        const trading::messaging::instrument_export_result& instrument) override;
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
    void populateFromInstrument();
    void emitProvenance();
    void onFieldChanged();

    Ui::CommodityInstrumentForm* ui_;
    ClientManager* clientManager_ = nullptr;
    std::string username_;
    trading::domain::commodity_instrument instrument_;
    bool dirty_ = false;
    bool loaded_ = false;
};

}

#endif
