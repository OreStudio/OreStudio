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
#ifndef ORES_QT_ACCOUNT_PARTIES_WIDGET_HPP
#define ORES_QT_ACCOUNT_PARTIES_WIDGET_HPP

#include <QWidget>
#include <QListWidget>
#include <QComboBox>
#include <QToolButton>
#include <QGroupBox>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.iam/domain/account_party.hpp"
#include "ores.refdata/domain/party.hpp"

namespace ores::qt {

/**
 * @brief Widget for managing parties assigned to an account.
 *
 * Displays the parties currently assigned to an account and allows
 * adding/removing party links via a combo box and toolbar buttons.
 */
class AccountPartiesWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.account_parties_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountPartiesWidget(QWidget* parent = nullptr);
    ~AccountPartiesWidget() override = default;

    /**
     * @brief Sets the client manager for making requests.
     */
    void setClientManager(ClientManager* clientManager);

    /**
     * @brief Sets the account ID to manage parties for.
     */
    void setAccountId(const boost::uuids::uuid& accountId);

    /**
     * @brief Loads the parties for the current account.
     */
    void loadParties();

    /**
     * @brief Sets the widget to read-only mode.
     */
    void setReadOnly(bool readOnly);

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& title, const QString& message);
    void partiesChanged();

private slots:
    void onAddPartyClicked();
    void onRemovePartyClicked();
    void onAssignedSelectionChanged();

private:
    void setupUi();
    void refreshView();
    void updateButtonStates();

    QGroupBox*   partiesGroup_;
    QListWidget* assignedList_;
    QComboBox*   partyCombo_;
    QToolButton* addButton_;
    QToolButton* removeButton_;

    ClientManager*     clientManager_ = nullptr;
    boost::uuids::uuid accountId_;
    bool               readOnly_ = false;

    std::vector<iam::domain::account_party>  assignedParties_;
    std::vector<refdata::domain::party>      allParties_;
};

}

#endif
