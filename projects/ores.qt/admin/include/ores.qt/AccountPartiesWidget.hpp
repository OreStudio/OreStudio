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

#include "ores.iam.api/domain/account_party.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.refdata.api/domain/party.hpp"
#include <QComboBox>
#include <QGroupBox>
#include <QLabel>
#include <QListWidget>
#include <QToolButton>
#include <QWidget>
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <vector>

namespace ores::qt {

/**
 * @brief Widget for managing parties assigned to an account.
 *
 * Displays the parties currently assigned to an account and allows
 * staging add/remove operations via a combo box and toolbar buttons.
 * Changes are committed by the parent dialog's Save action, which
 * handles the change reason dialog and calls loadParties() to reload.
 *
 * The widget emits partyListChanged() whenever the staged set changes,
 * so the parent dialog can enable its Save button accordingly.
 */
class AccountPartiesWidget : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.account_parties_widget";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit AccountPartiesWidget(QWidget* parent = nullptr);
    ~AccountPartiesWidget() override = default;

    void setClientManager(ClientManager* clientManager);
    void setAccountId(const boost::uuids::uuid& accountId);
    void setAccountType(const std::string& accountType);
    /**
     * @brief Marks which assigned party (if any) is the account's default
     * quick-login party. Pass a nil UUID to indicate no default is set.
     */
    void setDefaultPartyId(const boost::uuids::uuid& defaultPartyId);
    /**
     * @brief Load parties. If accountId is set, also fetches assigned parties.
     * If accountId is nil (create mode), only available parties are loaded.
     */
    void load();
    void setReadOnly(bool readOnly);

    [[nodiscard]] bool hasPendingChanges() const;
    [[nodiscard]] bool hasAvailableParties() const;
    /**
     * @brief True once the default-party combo has been seeded from the
     * account's actual stored default (i.e. load() has resolved at least
     * once for this account). False in the window between setAccountId()/
     * setDefaultPartyId() and load() completing. The parent dialog must
     * not allow Save while this is false in edit mode — saving before
     * this resolves would resend a not-yet-populated (nil) selection and
     * wipe the account's real default party.
     */
    [[nodiscard]] bool isDefaultPartyReady() const;
    [[nodiscard]] const std::vector<boost::uuids::uuid>& pendingAdds() const;
    [[nodiscard]] const std::vector<boost::uuids::uuid>& pendingRemoves() const;
    /**
     * @brief The account's currently-persisted party assignments, as loaded
     * by the last successful load(). Used by the parent dialog to populate
     * a "Default Party" selector from the same data, once dataLoaded() fires.
     */
    [[nodiscard]] const std::vector<iam::domain::account_party>& assignedParties() const;
    [[nodiscard]] const std::vector<refdata::domain::party>& allParties() const;
    /**
     * @brief The currently-selected value of the "Default Party" combo
     * (nil UUID for "(none)"). Read by the parent dialog's Save action.
     */
    [[nodiscard]] boost::uuids::uuid selectedDefaultPartyId() const;
    /**
     * @brief True when the "Default Party" combo's selection differs from
     * the value last set via setDefaultPartyId() — i.e. an unsaved change.
     */
    [[nodiscard]] bool hasPendingDefaultPartyChange() const;

signals:
    void statusMessage(const QString& message);
    void errorMessage(const QString& title, const QString& message);

    /**
     * @brief Emitted when the staged party list changes.
     *
     * The parent dialog connects to this signal to enable its Save button.
     */
    void partyListChanged();

    /**
     * @brief Emitted once load() completes successfully.
     */
    void dataLoaded();

    /**
     * @brief Emitted when the user changes the "Default Party" combo.
     *
     * The parent dialog connects to this signal to enable its Save button,
     * the same way it does for other account fields.
     */
    void defaultPartyChanged();

private slots:
    void onAddPartyClicked();
    void onRemovePartyClicked();
    void onAssignedSelectionChanged();

private:
    void setupUi();
    void refreshView();
    void populateDefaultPartyCombo();
    void updateButtonStates();

    QGroupBox* partiesGroup_;
    QListWidget* assignedList_;
    QComboBox* partyCombo_;
    QToolButton* addButton_;
    QToolButton* removeButton_;
    QLabel* defaultPartyLabel_;
    QComboBox* defaultPartyCombo_;

    ClientManager* clientManager_ = nullptr;
    boost::uuids::uuid accountId_;
    boost::uuids::uuid defaultPartyId_ = boost::uuids::nil_uuid();
    bool defaultPartyComboInitialized_ = false;
    std::string accountType_;
    bool readOnly_ = false;

    // DB state
    std::vector<iam::domain::account_party> assignedParties_;
    std::vector<refdata::domain::party> allParties_;

    // Pending local changes (committed by the parent dialog's Save action)
    std::vector<boost::uuids::uuid> pendingAdds_;
    std::vector<boost::uuids::uuid> pendingRemoves_;
};

}

#endif
