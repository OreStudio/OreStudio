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
#ifndef ORES_QT_HISTORY_DIALOG_HPP
#define ORES_QT_HISTORY_DIALOG_HPP

#include "ores.history.api/messaging/history_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/HistoryDialogBase.hpp"
#include <vector>

namespace Ui {
class HistoryDialog;
}

namespace ores::qt {

/**
 * @brief The single, non-templated history dialog shared by every
 * entity, per doc/knowledge/architecture/history_diff_architecture.org.
 *
 * Parameterised at construction by (entity_type, entity_id) rather
 * than subclassed per entity. Issues the generic history.v1.get
 * request and renders a master-detail, GitHub-style diff view per
 * doc/analysis/gemini_review_history_dialog.org: a compact version
 * timeline on the left, a live diff pane on the right with GitHub
 * colour highlighting (intra-line spans down to the token) and an
 * "All Fields"/"Only Changes" toggle. Two explicit Compare-From/
 * Compare-To selectors diff any two versions directly, not just
 * adjacent ones. No typed domain knowledge, no per-entity Qt class.
 * Open and Revert emit generic (entity_type, entity_id, version)
 * signals for the caller to resolve against that entity's own typed
 * request.
 */
class HistoryDialog final : public HistoryDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.history_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit HistoryDialog(std::string entityType,
                           std::string entityId,
                           ClientManager* clientManager,
                           QWidget* parent = nullptr);
    ~HistoryDialog() override;

    void loadHistory() override;
    [[nodiscard]] QString code() const override;

signals:
    void openVersionRequested(const QString& entityType, const QString& entityId, int version);
    void revertVersionRequested(const QString& entityType, const QString& entityId, int version);

protected:
    [[nodiscard]] int historySize() const override;
    [[nodiscard]] VersionRow versionRow(int index) const override;
    [[nodiscard]] QString historyTitle() const override;
    [[nodiscard]] bool supportsCompareMode() const override {
        return true;
    }
    [[nodiscard]] DiffResult
    calculateDiffBetween(int index_new, int index_old, bool include_unchanged) const override;
    void openVersionAt(int index) override;
    void revertToVersionAt(int index) override;

private:
    Ui::HistoryDialog* ui_;
    std::string entityType_;
    std::string entityId_;
    ClientManager* clientManager_;
    std::vector<ores::history::messaging::entity_history_version> versions_;
};

}

#endif
