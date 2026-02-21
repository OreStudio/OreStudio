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
#ifndef ORES_QT_PARTYPICKERDIALOG_HPP
#define ORES_QT_PARTYPICKERDIALOG_HPP

#include <vector>
#include <QDialog>
#include <QLabel>
#include <QListWidget>
#include <QPushButton>
#include <boost/uuid/uuid.hpp>
#include "ores.qt/ClientManager.hpp"

namespace ores::qt {

/**
 * @brief Modal dialog for selecting a party from a list of available parties.
 *
 * Shown after login when the account is associated with multiple parties.
 * The user must select exactly one party to bind the session context.
 */
class PartyPickerDialog : public QDialog {
    Q_OBJECT

public:
    /**
     * @brief Construct PartyPickerDialog.
     *
     * @param parties The list of parties to choose from.
     * @param clientManager Pointer to the application's client manager.
     * @param parent Parent widget.
     */
    explicit PartyPickerDialog(
        const std::vector<PartyInfo>& parties,
        ClientManager* clientManager,
        QWidget* parent = nullptr);

    /**
     * @brief Get the UUID of the selected party.
     */
    boost::uuids::uuid selectedPartyId() const;

    /**
     * @brief Get the name of the selected party.
     */
    QString selectedPartyName() const;

private slots:
    void onOkClicked();

private:
    void setupUi();

private:
    QListWidget*   listWidget_;
    QPushButton*   okButton_;
    QPushButton*   cancelButton_;
    ClientManager* clientManager_;
    std::vector<PartyInfo> parties_;
};

}

#endif
