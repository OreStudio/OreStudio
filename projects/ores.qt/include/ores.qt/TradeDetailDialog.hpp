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
#ifndef ORES_QT_TRADE_DETAIL_DIALOG_HPP
#define ORES_QT_TRADE_DETAIL_DIALOG_HPP

#include <vector>
#include <QTabWidget>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/ProvenanceWidget.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/fx_instrument.hpp"
#include "ores.trading.api/domain/instrument.hpp"
#include "ores.trading.api/domain/swap_leg.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/credit_instrument.hpp"
#include "ores.trading.api/domain/equity_instrument.hpp"
#include "ores.trading.api/domain/commodity_instrument.hpp"
#include "ores.trading.api/domain/composite_instrument.hpp"
#include "ores.trading.api/domain/composite_leg.hpp"
#include "ores.trading.api/domain/scripted_instrument.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.refdata.api/domain/counterparty.hpp"

namespace Ui {
class TradeDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing a trade and its linked
 *        instrument.
 *
 * The dialog contains trade tabs (General, Dates) followed by
 * instrument-family tabs that are revealed once the instrument loads
 * asynchronously. A single Provenance tab holds two sections — Trade and
 * Instrument — so that both audit trails are visible in one place.
 *
 * Save semantics: if the instrument is dirty, it is saved first; the trade
 * is then always saved so that any instrument change is reflected in the
 * trade version number (downstream notification invariant).
 */
class TradeDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.trade_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit TradeDetailDialog(QWidget* parent = nullptr);
    ~TradeDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setTrade(const trading::domain::trade& trade);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

protected:
    QTabWidget*       tabWidget()        const override;
    QWidget*          provenanceTab()    const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override {
        return hasChanges_ || instrumentHasChanges_;
    }

signals:
    void tradeSaved(const QString& code);
    void tradeDeleted(const QString& code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();
    void onInstrumentFieldChanged();
    void onFxTradeTypeChanged(const QString& text);
    void onSwapTradeTypeChanged(const QString& text);
    void onBondTradeTypeChanged(const QString& text);
    void onCreditTradeTypeChanged(const QString& text);
    void onEquityTradeTypeChanged(const QString& text);
    void onCommodityTradeTypeChanged(const QString& text);
    void onCompositeTradeTypeChanged(const QString& text);
    void onScriptTradeTypeChanged(const QString& text);

private:
    void setupUi();
    void setupConnections();
    void loadBooks();
    void selectCurrentBook();
    void loadCounterparties();
    void selectCurrentCounterparty();
    void updateUiFromTrade();
    void updateTradeFromUi();
    void updateSaveButtonState();
    bool validateInput();

    // FX instrument support
    void loadFxInstrument();
    void populateFxInstrument();
    void updateFxInstrumentFromUi();
    void updateFxTabVisibility();
    void setFxReadOnly(bool readOnly);
    void saveFxThenTrade(const trading::domain::trade& trade,
                         const trading::domain::fx_instrument& instrument);

    // Swap / Rates instrument support
    void loadSwapInstrument();
    void populateSwapInstrument();
    void updateSwapInstrumentFromUi();
    void updateSwapTabVisibility();
    void setSwapReadOnly(bool readOnly);
    void saveSwapThenTrade(const trading::domain::trade& trade,
                           const trading::domain::instrument& instrument,
                           const std::vector<trading::domain::swap_leg>& legs);

    // Bond instrument support
    void loadBondInstrument();
    void populateBondInstrument();
    void updateBondInstrumentFromUi();
    void updateBondTabVisibility();
    void setBondReadOnly(bool readOnly);
    void saveBondThenTrade(const trading::domain::trade& trade,
                           const trading::domain::bond_instrument& instrument);

    // Credit instrument support
    void loadCreditInstrument();
    void populateCreditInstrument();
    void updateCreditInstrumentFromUi();
    void updateCreditTabVisibility();
    void setCreditReadOnly(bool readOnly);
    void saveCreditThenTrade(const trading::domain::trade& trade,
                             const trading::domain::credit_instrument& instrument);

    // Equity instrument support
    void loadEquityInstrument();
    void populateEquityInstrument();
    void updateEquityInstrumentFromUi();
    void updateEquityTabVisibility();
    void setEquityReadOnly(bool readOnly);
    void saveEquityThenTrade(const trading::domain::trade& trade,
                             const trading::domain::equity_instrument& instrument);

    // Commodity instrument support
    void loadCommodityInstrument();
    void populateCommodityInstrument();
    void updateCommodityInstrumentFromUi();
    void updateCommodityTabVisibility();
    void setCommodityReadOnly(bool readOnly);
    void saveCommodityThenTrade(const trading::domain::trade& trade,
                                const trading::domain::commodity_instrument& instrument);

    // Composite instrument support
    void loadCompositeInstrument();
    void populateCompositeInstrument();
    void updateCompositeInstrumentFromUi();
    void updateCompositeTabVisibility();
    void setCompositeReadOnly(bool readOnly);
    void saveCompositeThenTrade(const trading::domain::trade& trade,
                                const trading::domain::composite_instrument& instrument,
                                const std::vector<trading::domain::composite_leg>& legs);

    // Scripted instrument support
    void loadScriptedInstrument();
    void populateScriptedInstrument();
    void updateScriptedInstrumentFromUi();
    void updateScriptedTabVisibility();
    void setScriptedReadOnly(bool readOnly);
    void saveScriptedThenTrade(const trading::domain::trade& trade,
                               const trading::domain::scripted_instrument& instrument);

    void saveTrade(const trading::domain::trade& trade);

    Ui::TradeDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    trading::domain::trade trade_;
    std::vector<refdata::domain::book> books_;
    std::vector<refdata::domain::counterparty> counterparties_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};

    // Instrument state (at most one family is loaded at a time)
    trading::domain::fx_instrument fxInstrument_;
    trading::domain::instrument swapInstrument_;
    std::vector<trading::domain::swap_leg> swapLegs_;
    trading::domain::bond_instrument bondInstrument_;
    trading::domain::credit_instrument creditInstrument_;
    trading::domain::equity_instrument equityInstrument_;
    trading::domain::commodity_instrument commodityInstrument_;
    trading::domain::composite_instrument compositeInstrument_;
    std::vector<trading::domain::composite_leg> compositeLegs_;
    trading::domain::scripted_instrument scriptedInstrument_;
    bool instrumentLoaded_{false};
    bool instrumentHasChanges_{false};
};

}

#endif
