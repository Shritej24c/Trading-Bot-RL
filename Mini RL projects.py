import gym
from stable_baselines3 import DQN, A2C, PPO, DDPG

env = gym.make("LunarLander-v2")

print(env.action_space.sample())
print(env.observation_space.sample())

model = A2C("MlpPolicy", env, verbose = 1)
model.learn(total_timesteps=10000)

state = env.reset()
while True:
    action, _states = model.predict(state)
    obs, reward, done, info = env.step(action)
    env.render(mode='human')

