/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_QT_ACCOUNT_HISTORY_DIALOG_HPP
#define ORES_QT_ACCOUNT_HISTORY_DIALOG_HPP

#include "ores.iam.api/domain/account_version.hpp"
#include "ores.iam.api/messaging/account_history_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include <QString>
#include <memory>

namespace Ui {
class AccountHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Widget for displaying account version history.
 */
class AccountHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.account_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountHistoryDialog(QString username,
                                  ClientManager* clientManager,
                                  QWidget* parent = nullptr);
    ~AccountHistoryDialog() override;

    void loadHistory() override;

    /**
     * @brief Returns the username of the account.
     */
    [[nodiscard]] QString code() const override {
        return username_;
    }

signals:
    /**
     * @brief Emitted when user requests to open a version in read-only mode.
     * @param account The account data at the selected version.
     * @param versionNumber The version number being viewed.
     */
    void openVersionRequested(const iam::domain::account& account, int versionNumber);

    /**
     * @brief Emitted when user requests to revert to a selected version.
     * @param account The account data to revert to.
     */
    void revertVersionRequested(const iam::domain::account& account);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult
    calculateDiffAt(int current_index, int previous_index) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    std::unique_ptr<Ui::AccountHistoryDialog> ui_;
    ClientManager* clientManager_;
    QString username_;
    iam::messaging::account_version_history history_;
};

}

#endif
