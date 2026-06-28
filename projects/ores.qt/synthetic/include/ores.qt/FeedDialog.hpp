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
#ifndef ORES_QT_FEED_DIALOG_HPP
#define ORES_QT_FEED_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.synthetic.api/domain/market_data_generation_config.hpp"
#include <QCheckBox>
#include <QDialog>
#include <QLineEdit>
#include <QPlainTextEdit>
#include <string>

namespace ores::qt {

/**
 * @brief Modal dialog for creating or editing a market data feed.
 *
 * A feed is a market_data_generation_config: a named simulation that owns one
 * or more FX pairs. The dialog persists the feed on Save (off the UI thread)
 * and only calls accept() once the server confirms success, so the owning
 * window can reload when exec() returns QDialog::Accepted.
 */
class FeedDialog final : public QDialog {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.synthetic.feed_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a dialog for a new feed.
     */
    explicit FeedDialog(ClientManager* cm, const QString& username, QWidget* parent = nullptr);

    /**
     * @brief Construct a dialog editing an existing feed.
     */
    FeedDialog(ClientManager* cm,
               const QString& username,
               const synthetic::domain::market_data_generation_config& existing,
               QWidget* parent = nullptr);

    ~FeedDialog() override = default;

private slots:
    void onSave();

private:
    void buildUi();

    ClientManager* clientManager_;
    QString username_;
    bool isNew_;
    synthetic::domain::market_data_generation_config feed_;

    QLineEdit* nameEdit_;
    QPlainTextEdit* descEdit_;
    QCheckBox* enabledCheck_;
};

}

#endif
