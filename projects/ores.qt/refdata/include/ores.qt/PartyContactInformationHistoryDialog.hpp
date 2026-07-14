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
#ifndef ORES_QT_PARTY_CONTACT_INFORMATION_HISTORY_DIALOG_HPP
#define ORES_QT_PARTY_CONTACT_INFORMATION_HISTORY_DIALOG_HPP

#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include "ores.refdata.api/domain/party_contact_information.hpp"
#include <boost/uuid/uuid.hpp>

namespace Ui {
class PartyContactInformationHistoryDialog;
}

namespace ores::qt {

/**
 * @brief Dialog for viewing the version history of a party contact information.
 *
 * Shows all historical versions of a party contact information with ability
 * to view details or revert to a previous version.
 */
class PartyContactInformationHistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.party_contact_information_history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit PartyContactInformationHistoryDialog(const boost::uuids::uuid& id,
                                                  const QString& code,
                                                  ClientManager* clientManager,
                                                  QWidget* parent = nullptr);
    ~PartyContactInformationHistoryDialog() override;

    void loadHistory() override;
    [[nodiscard]] QString code() const override;

signals:
    void
    openVersionRequested(const refdata::domain::party_contact_information& partyContactInformation,
                         int versionNumber);
    void revertVersionRequested(
        const refdata::domain::party_contact_information& partyContactInformation);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] DiffResult calculateDiffAt(int ci, int pi) const override;
    void displayFullDetails(int index) override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    Ui::PartyContactInformationHistoryDialog* ui_;
    boost::uuids::uuid id_;
    QString code_;
    ClientManager* clientManager_;
    std::vector<refdata::domain::party_contact_information> versions_;
};

}

#endif
