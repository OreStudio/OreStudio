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
#ifndef ORES_QT_MARKET_DATA_GENERATION_CONFIG_DETAIL_DIALOG_HPP
#define ORES_QT_MARKET_DATA_GENERATION_CONFIG_DETAIL_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"

namespace Ui {
class MarketDataGenerationConfigDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing market data generation config records.
 *
 * This dialog allows viewing, creating, and editing market data generation
 * configs. It supports both create mode (for new records) and edit mode (for
 * existing records).
 */
class MarketDataGenerationConfigDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.market_data_generation_config_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit MarketDataGenerationConfigDetailDialog(QWidget* parent = nullptr);
    ~MarketDataGenerationConfigDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setUsername(const std::string& username);
    void setConfig(const synthetic::domain::market_data_generation_config& config);
    void setCreateMode(bool createMode);

signals:
    void configCreated(const QString& id);
    void configUpdated(const QString& id);

private slots:
    void onSaveClicked();
    void onFieldChanged();

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;
    bool hasUnsavedChanges() const override {
        return hasChanges_;
    }

private:
    void setupUi();
    void setupConnections();
    void updateUiFromConfig();
    void updateConfigFromUi();
    void updateSaveButtonState();
    bool validateInput();

    Ui::MarketDataGenerationConfigDetailDialog* ui_;
    ClientManager* clientManager_;
    std::string username_;
    synthetic::domain::market_data_generation_config config_;
    bool createMode_{true};
    bool hasChanges_{false};
};

}

#endif
