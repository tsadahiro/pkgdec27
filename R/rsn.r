#' Compute particle trajectories from a reduced word
#'
#' Given a reduced word \code{w} of a (type B) sorting network,
#' this function computes the trajectory of selected particles.
#' Only event times at which the specified particles move are recorded.
#'
#' @param w An integer vector representing a reduced word.
#' @param particles An integer vector specifying particle indices to track.
#'
#' @return A list with components \code{time} and \code{pos}.
#'
#'
#' @details
#' The function is designed for large-scale simulations.
#' It records only event times for the specified particles,
#' making it memory-efficient even for very long reduced words.
#'
#' @examples
#' w <- shifted_hook_walk_xtrace(30)
#' tr <- trajectory(w, particles = c(1, seq(5, 30, by = 5)))
#' tr$pos[[1]]
#'
#' @export
trajectory = function(w, particles) {
        n = max(w) + 1
        # sheet -> person; person[i] is the person who sit on the sheet i
        person = 1:n
        # person -> sheet; sheet[j] is the sheet on which the person j sit on
        sheet = 1:n

        track <- rep(FALSE, n)
        track[particles] <- TRUE

        T = length(w)
        time = vector("list", length(particles))
        pos = vector("list", length(particles))
        idx = integer(n)
        idx[particles] = seq_along(particles)
        for (p in particles) {
                j = idx[p]
                time[[j]] = c(0)
                pos[[j]] = c(p)
        }
        for (t in 1:T) {
                if (w[t] == 0) {
                        person[1] = -person[1]
                } else {
                        p = abs(person[w[t]])
                        q = abs(person[w[t] + 1])
                        sheet[p] = sheet[p] + 1
                        sheet[q] = sheet[q] - 1
                        if (track[p]) {
                                j = idx[p]
                                time[[j]] = c(time[[j]], t)
                                pos[[j]] = c(pos[[j]], sheet[p])
                        }
                        if (track[q]) {
                                j = idx[q]
                                time[[j]] = c(time[[j]], t)
                                pos[[j]] = c(pos[[j]], sheet[q])
                        }
                        buf = person[w[t]]
                        person[w[t]] = person[w[t] + 1]
                        person[w[t] + 1] = buf
                }
        }
        for (p in particles) {
                j = idx[p]
                time[[j]] = c(time[[j]], T)
                pos[[j]] = c(pos[[j]], p)
        }
        return(list(time = time, pos = pos, particles = particles))
}


#' Plot particle trajectories
#'
#' @param traj trajectory object
#' @return NULL
#'
#' @importFrom grDevices hsv
#' @importFrom graphics lines
#' @examples
#' w <- shifted_hook_walk_xtrace(30)
#' tr <- trajectory(w, particles = c(1, 5 * (1:6)))
#' plot_trajectory(tr)
#' @export
plot_trajectory = function(traj){
  m = length(traj$time)
  n = max(unlist(traj$pos))
  T = n^2
  plot(NA,
       xlim = c(0, T), ylim = c(0, n),
       xlab = "time", ylab = "position"
  )
  cols = hsv(
    h = (traj$particles - 1) / (n - 1),  # 0〜1 に正規化
    s = 1,
    v = 1
  )
  for (i in 1:m) {
    lines(traj$time[[i]], traj$pos[[i]], col=cols[i])
  }
}
