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
#ifndef ORES_QT_FX_PAIR_DIALOG_HPP
#define ORES_QT_FX_PAIR_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/fx_spot_generation_config.hpp"
#include <QCheckBox>
#include <QComboBox>
#include <QDialog>
#include <QDoubleSpinBox>
#include <QLabel>
#include <QSpinBox>
#include <string>
#include <vector>

namespace ores::qt {

class ImageCache;

/**
 * @brief Modal dialog for creating or editing an FX pair within a feed.
 *
 * An FX pair is an fx_spot_generation_config: the currency pair to simulate
 * (e.g. EUR/USD). The ORE key and source name are derived from the chosen
 * currencies and shown read-only. The dialog persists the config on Save (off
 * the UI thread) and only calls accept() once the server confirms success.
 */
class FxPairDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.fx_pair_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a dialog for a new FX pair under @p parentFeedId.
     */
    FxPairDialog(ClientManager* cm,
                 const QString& username,
                 ImageCache* imageCache,
                 const boost::uuids::uuid& parentFeedId,
                 QWidget* parent = nullptr);

    /**
     * @brief Construct a dialog editing an existing FX pair.
     */
    FxPairDialog(ClientManager* cm,
                 const QString& username,
                 ImageCache* imageCache,
                 const synthetic::domain::fx_spot_generation_config& existing,
                 QWidget* parent = nullptr);

    ~FxPairDialog() override = default;

private slots:
    void onSave();
    void onCurrencyChanged();

private:
    void buildUi();
    void populateCurrencyCombo(QComboBox* combo);
    void recomputeDerived();

    ClientManager* clientManager_;
    QString username_;
    ImageCache* imageCache_;
    bool isNew_;
    synthetic::domain::fx_spot_generation_config fx_;

    QComboBox* baseCombo_;
    QComboBox* quoteCombo_;
    std::vector<std::string> knownCodes_;
    QLabel* oreKeyLabel_;
    QLabel* sourceNameLabel_;
    QDoubleSpinBox* priceSpin_;
    QSpinBox* ticksSpin_;
    QCheckBox* enabledCheck_;
};

}

#endif
